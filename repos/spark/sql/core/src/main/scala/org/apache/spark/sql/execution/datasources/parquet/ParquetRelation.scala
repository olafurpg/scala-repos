/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.sql.execution.datasources.parquet

import java.net.URI
import java.util.{List => JList}
import java.util.logging.{Logger => JLogger}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{Failure, Try}

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileStatus, Path}
import org.apache.hadoop.io.Writable
import org.apache.hadoop.mapreduce._
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat
import org.apache.hadoop.mapreduce.task.JobContextImpl
import org.apache.parquet.{Log => ApacheParquetLog}
import org.apache.parquet.filter2.predicate.FilterApi
import org.apache.parquet.hadoop._
import org.apache.parquet.hadoop.metadata.CompressionCodecName
import org.apache.parquet.hadoop.util.ContextUtil
import org.apache.parquet.schema.MessageType
import org.slf4j.bridge.SLF4JBridgeHandler

import org.apache.spark.{Partition => SparkPartition, SparkException}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.internal.Logging
import org.apache.spark.rdd.{RDD, SqlNewHadoopPartition, SqlNewHadoopRDD}
import org.apache.spark.sql._
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.parser.LegacyTypeStringParser
import org.apache.spark.sql.execution.datasources.{PartitionSpec, _}
import org.apache.spark.sql.internal.SQLConf
import org.apache.spark.sql.sources._
import org.apache.spark.sql.types.{DataType, StructType}
import org.apache.spark.util.{SerializableConfiguration, Utils}
import org.apache.spark.util.collection.BitSet

private[sql] class DefaultSource
    extends FileFormat
    with DataSourceRegister
    with Logging {

  override def shortName(): String = "parquet"

  override def toString: String = "ParquetFormat"

  override def equals(other: Any): Boolean = other.isInstanceOf[DefaultSource]

  override def prepareWrite(sqlContext: SQLContext,
                            job: Job,
                            options: Map[String, String],
                            dataSchema: StructType): OutputWriterFactory = {

    val conf = ContextUtil.getConfiguration(job)

    // SPARK-9849 DirectParquetOutputCommitter qualified name should be backward compatible
    val committerClassName =
      conf.get(SQLConf.PARQUET_OUTPUT_COMMITTER_CLASS.key)
    if (committerClassName == "org.apache.spark.sql.parquet.DirectParquetOutputCommitter") {
      conf.set(SQLConf.PARQUET_OUTPUT_COMMITTER_CLASS.key,
               classOf[DirectParquetOutputCommitter].getCanonicalName)
    }

    val committerClass = conf.getClass(
        SQLConf.PARQUET_OUTPUT_COMMITTER_CLASS.key,
        classOf[ParquetOutputCommitter],
        classOf[ParquetOutputCommitter])

    if (conf.get(SQLConf.PARQUET_OUTPUT_COMMITTER_CLASS.key) == null) {
      logInfo(
          "Using default output committer for Parquet: " +
            classOf[ParquetOutputCommitter].getCanonicalName)
    } else {
      logInfo(
          "Using user defined output committer for Parquet: " +
            committerClass.getCanonicalName)
    }

    val compressionCodec: Option[String] = options.get("compression").map {
      codecName =>
        // Validate if given compression codec is supported or not.
        val shortParquetCompressionCodecNames =
          ParquetRelation.shortParquetCompressionCodecNames
        if (!shortParquetCompressionCodecNames
              .contains(codecName.toLowerCase)) {
          val availableCodecs =
            shortParquetCompressionCodecNames.keys.map(_.toLowerCase)
          throw new IllegalArgumentException(s"Codec [$codecName] " +
                s"is not available. Available codecs are ${availableCodecs.mkString(", ")}.")
        }
        codecName.toLowerCase
    }

    conf.setClass(SQLConf.OUTPUT_COMMITTER_CLASS.key,
                  committerClass,
                  classOf[ParquetOutputCommitter])

    // We're not really using `ParquetOutputFormat[Row]` for writing data here, because we override
    // it in `ParquetOutputWriter` to support appending and dynamic partitioning.  The reason why
    // we set it here is to setup the output committer class to `ParquetOutputCommitter`, which is
    // bundled with `ParquetOutputFormat[Row]`.
    job.setOutputFormatClass(classOf[ParquetOutputFormat[Row]])

    ParquetOutputFormat
      .setWriteSupportClass(job, classOf[CatalystWriteSupport])

    // We want to clear this temporary metadata from saving into Parquet file.
    // This metadata is only useful for detecting optional columns when pushdowning filters.
    val dataSchemaToWrite = StructType
      .removeMetadata(StructType.metadataKeyForOptionalField, dataSchema)
      .asInstanceOf[StructType]
    CatalystWriteSupport.setSchema(dataSchemaToWrite, conf)

    // Sets flags for `CatalystSchemaConverter` (which converts Catalyst schema to Parquet schema)
    // and `CatalystWriteSupport` (writing actual rows to Parquet files).
    conf.set(SQLConf.PARQUET_BINARY_AS_STRING.key,
             sqlContext.conf.isParquetBinaryAsString.toString)

    conf.set(SQLConf.PARQUET_INT96_AS_TIMESTAMP.key,
             sqlContext.conf.isParquetINT96AsTimestamp.toString)

    conf.set(SQLConf.PARQUET_WRITE_LEGACY_FORMAT.key,
             sqlContext.conf.writeLegacyParquetFormat.toString)

    // Sets compression scheme
    conf.set(
        ParquetOutputFormat.COMPRESSION,
        ParquetRelation.shortParquetCompressionCodecNames
          .getOrElse(compressionCodec.getOrElse(
                         sqlContext.conf.parquetCompressionCodec.toLowerCase),
                     CompressionCodecName.UNCOMPRESSED)
          .name())

    new OutputWriterFactory {
      override def newInstance(path: String,
                               bucketId: Option[Int],
                               dataSchema: StructType,
                               context: TaskAttemptContext): OutputWriter = {
        new ParquetOutputWriter(path, bucketId, context)
      }
    }
  }

  def inferSchema(sqlContext: SQLContext,
                  parameters: Map[String, String],
                  files: Seq[FileStatus]): Option[StructType] = {
    // Should we merge schemas from all Parquet part-files?
    val shouldMergeSchemas = parameters
      .get(ParquetRelation.MERGE_SCHEMA)
      .map(_.toBoolean)
      .getOrElse(
          sqlContext.conf.getConf(SQLConf.PARQUET_SCHEMA_MERGING_ENABLED))

    val mergeRespectSummaries =
      sqlContext.conf.getConf(SQLConf.PARQUET_SCHEMA_RESPECT_SUMMARIES)

    val filesByType = splitFiles(files)

    // Sees which file(s) we need to touch in order to figure out the schema.
    //
    // Always tries the summary files first if users don't require a merged schema.  In this case,
    // "_common_metadata" is more preferable than "_metadata" because it doesn't contain row
    // groups information, and could be much smaller for large Parquet files with lots of row
    // groups.  If no summary file is available, falls back to some random part-file.
    //
    // NOTE: Metadata stored in the summary files are merged from all part-files.  However, for
    // user defined key-value metadata (in which we store Spark SQL schema), Parquet doesn't know
    // how to merge them correctly if some key is associated with different values in different
    // part-files.  When this happens, Parquet simply gives up generating the summary file.  This
    // implies that if a summary file presents, then:
    //
    //   1. Either all part-files have exactly the same Spark SQL schema, or
    //   2. Some part-files don't contain Spark SQL schema in the key-value metadata at all (thus
    //      their schemas may differ from each other).
    //
    // Here we tend to be pessimistic and take the second case into account.  Basically this means
    // we can't trust the summary files if users require a merged schema, and must touch all part-
    // files to do the merge.
    val filesToTouch = if (shouldMergeSchemas) {
      // Also includes summary files, 'cause there might be empty partition directories.

      // If mergeRespectSummaries config is true, we assume that all part-files are the same for
      // their schema with summary files, so we ignore them when merging schema.
      // If the config is disabled, which is the default setting, we merge all part-files.
      // In this mode, we only need to merge schemas contained in all those summary files.
      // You should enable this configuration only if you are very sure that for the parquet
      // part-files to read there are corresponding summary files containing correct schema.

      // As filed in SPARK-11500, the order of files to touch is a matter, which might affect
      // the ordering of the output columns. There are several things to mention here.
      //
      //  1. If mergeRespectSummaries config is false, then it merges schemas by reducing from
      //     the first part-file so that the columns of the lexicographically first file show
      //     first.
      //
      //  2. If mergeRespectSummaries config is true, then there should be, at least,
      //     "_metadata"s for all given files, so that we can ensure the columns of
      //     the lexicographically first file show first.
      //
      //  3. If shouldMergeSchemas is false, but when multiple files are given, there is
      //     no guarantee of the output order, since there might not be a summary file for the
      //     lexicographically first file, which ends up putting ahead the columns of
      //     the other files. However, this should be okay since not enabling
      //     shouldMergeSchemas means (assumes) all the files have the same schemas.

      val needMerged: Seq[FileStatus] = if (mergeRespectSummaries) {
        Seq()
      } else {
        filesByType.data
      }
      needMerged ++ filesByType.metadata ++ filesByType.commonMetadata
    } else {
      // Tries any "_common_metadata" first. Parquet files written by old versions or Parquet
      // don't have this.
      filesByType.commonMetadata.headOption
      // Falls back to "_metadata"
        .orElse(filesByType.metadata.headOption)
        // Summary file(s) not found, the Parquet file is either corrupted, or different part-
        // files contain conflicting user defined metadata (two or more values are associated
        // with a same key in different files).  In either case, we fall back to any of the
        // first part-file, and just assume all schemas are consistent.
        .orElse(filesByType.data.headOption)
        .toSeq
    }
    ParquetRelation.mergeSchemasInParallel(filesToTouch, sqlContext)
  }

  case class FileTypes(data: Seq[FileStatus],
                       metadata: Seq[FileStatus],
                       commonMetadata: Seq[FileStatus])

  private def splitFiles(allFiles: Seq[FileStatus]): FileTypes = {
    // Lists `FileStatus`es of all leaf nodes (files) under all base directories.
    val leaves = allFiles.filter { f =>
      isSummaryFile(f.getPath) ||
      !(f.getPath.getName.startsWith("_") || f.getPath.getName.startsWith("."))
    }.toArray.sortBy(_.getPath.toString)

    FileTypes(
        data = leaves.filterNot(f => isSummaryFile(f.getPath)),
        metadata = leaves.filter(
            _.getPath.getName == ParquetFileWriter.PARQUET_METADATA_FILE),
        commonMetadata = leaves.filter(
            _.getPath.getName == ParquetFileWriter.PARQUET_COMMON_METADATA_FILE))
  }

  private def isSummaryFile(file: Path): Boolean = {
    file.getName == ParquetFileWriter.PARQUET_COMMON_METADATA_FILE ||
    file.getName == ParquetFileWriter.PARQUET_METADATA_FILE
  }

  override def buildInternalScan(
      sqlContext: SQLContext,
      dataSchema: StructType,
      requiredColumns: Array[String],
      filters: Array[Filter],
      bucketSet: Option[BitSet],
      allFiles: Seq[FileStatus],
      broadcastedConf: Broadcast[SerializableConfiguration],
      options: Map[String, String]): RDD[InternalRow] = {
    val useMetadataCache = sqlContext.getConf(SQLConf.PARQUET_CACHE_METADATA)
    val parquetFilterPushDown = sqlContext.conf.parquetFilterPushDown
    val assumeBinaryIsString = sqlContext.conf.isParquetBinaryAsString
    val assumeInt96IsTimestamp = sqlContext.conf.isParquetINT96AsTimestamp

    // Parquet row group size. We will use this value as the value for
    // mapreduce.input.fileinputformat.split.minsize and mapred.min.split.size if the value
    // of these flags are smaller than the parquet row group size.
    val parquetBlockSize =
      ParquetOutputFormat.getLongBlockSize(broadcastedConf.value.value)

    // Create the function to set variable Parquet confs at both driver and executor side.
    val initLocalJobFuncOpt = ParquetRelation.initializeLocalJobFunc(
        requiredColumns,
        filters,
        dataSchema,
        parquetBlockSize,
        useMetadataCache,
        parquetFilterPushDown,
        assumeBinaryIsString,
        assumeInt96IsTimestamp) _

    val inputFiles = splitFiles(allFiles).data.toArray

    // Create the function to set input paths at the driver side.
    val setInputPaths = ParquetRelation
      .initializeDriverSideJobFunc(inputFiles, parquetBlockSize) _

    Utils.withDummyCallSite(sqlContext.sparkContext) {
      new SqlNewHadoopRDD(sqlContext = sqlContext,
                          broadcastedConf = broadcastedConf,
                          initDriverSideJobFuncOpt = Some(setInputPaths),
                          initLocalJobFuncOpt = Some(initLocalJobFuncOpt),
                          inputFormatClass =
                            classOf[ParquetInputFormat[InternalRow]],
                          valueClass = classOf[InternalRow]) {

        val cacheMetadata = useMetadataCache

        @transient val cachedStatuses = inputFiles.map { f =>
          // In order to encode the authority of a Path containing special characters such as '/'
          // (which does happen in some S3N credentials), we need to use the string returned by the
          // URI of the path to create a new Path.
          val pathWithEscapedAuthority = escapePathUserInfo(f.getPath)
          new FileStatus(f.getLen,
                         f.isDirectory,
                         f.getReplication,
                         f.getBlockSize,
                         f.getModificationTime,
                         f.getAccessTime,
                         f.getPermission,
                         f.getOwner,
                         f.getGroup,
                         pathWithEscapedAuthority)
        }.toSeq

        private def escapePathUserInfo(path: Path): Path = {
          val uri = path.toUri
          new Path(
              new URI(uri.getScheme,
                      uri.getRawUserInfo,
                      uri.getHost,
                      uri.getPort,
                      uri.getPath,
                      uri.getQuery,
                      uri.getFragment))
        }

        // Overridden so we can inject our own cached files statuses.
        override def getPartitions: Array[SparkPartition] = {
          val inputFormat = new ParquetInputFormat[InternalRow] {
            override def listStatus(
                jobContext: JobContext): JList[FileStatus] = {
              if (cacheMetadata) cachedStatuses.asJava
              else super.listStatus(jobContext)
            }
          }

          val jobContext =
            new JobContextImpl(getConf(isDriverSide = true), jobId)
          val rawSplits = inputFormat.getSplits(jobContext)

          Array.tabulate[SparkPartition](rawSplits.size) { i =>
            new SqlNewHadoopPartition(
                id,
                i,
                rawSplits.get(i).asInstanceOf[InputSplit with Writable])
          }
        }
      }
    }
  }
}

// NOTE: This class is instantiated and used on executor side only, no need to be serializable.
private[sql] class ParquetOutputWriter(path: String,
                                       bucketId: Option[Int],
                                       context: TaskAttemptContext)
    extends OutputWriter {

  private val recordWriter: RecordWriter[Void, InternalRow] = {
    val outputFormat = {
      new ParquetOutputFormat[InternalRow]() {
        // Here we override `getDefaultWorkFile` for two reasons:
        //
        //  1. To allow appending.  We need to generate unique output file names to avoid
        //     overwriting existing files (either exist before the write job, or are just written
        //     by other tasks within the same write job).
        //
        //  2. To allow dynamic partitioning.  Default `getDefaultWorkFile` uses
        //     `FileOutputCommitter.getWorkPath()`, which points to the base directory of all
        //     partitions in the case of dynamic partitioning.
        override def getDefaultWorkFile(context: TaskAttemptContext,
                                        extension: String): Path = {
          val configuration = context.getConfiguration
          val uniqueWriteJobId =
            configuration.get("spark.sql.sources.writeJobUUID")
          val taskAttemptId = context.getTaskAttemptID
          val split = taskAttemptId.getTaskID.getId
          val bucketString =
            bucketId.map(BucketingUtils.bucketIdToString).getOrElse("")
          // It has the `.parquet` extension at the end because (de)compression tools
          // such as gunzip would not be able to decompress this as the compression
          // is not applied on this whole file but on each "page" in Parquet format.
          new Path(
              path,
              f"part-r-$split%05d-$uniqueWriteJobId$bucketString$extension")
        }
      }
    }

    outputFormat.getRecordWriter(context)
  }

  override def write(row: Row): Unit =
    throw new UnsupportedOperationException("call writeInternal")

  override protected[sql] def writeInternal(row: InternalRow): Unit =
    recordWriter.write(null, row)

  override def close(): Unit = recordWriter.close(context)
}

private[sql] object ParquetRelation extends Logging {
  // Whether we should merge schemas collected from all Parquet part-files.
  private[sql] val MERGE_SCHEMA = "mergeSchema"

  // Hive Metastore schema, used when converting Metastore Parquet tables.  This option is only used
  // internally.
  private[sql] val METASTORE_SCHEMA = "metastoreSchema"

  // If a ParquetRelation is converted from a Hive metastore table, this option is set to the
  // original Hive table name.
  private[sql] val METASTORE_TABLE_NAME = "metastoreTableName"

  /**
    * If parquet's block size (row group size) setting is larger than the min split size,
    * we use parquet's block size setting as the min split size. Otherwise, we will create
    * tasks processing nothing (because a split does not cover the starting point of a
    * parquet block). See https://issues.apache.org/jira/browse/SPARK-10143 for more information.
    */
  private def overrideMinSplitSize(parquetBlockSize: Long,
                                   conf: Configuration): Unit = {
    val minSplitSize = math.max(
        conf.getLong("mapred.min.split.size", 0L),
        conf.getLong("mapreduce.input.fileinputformat.split.minsize", 0L))
    if (parquetBlockSize > minSplitSize) {
      val message =
        s"Parquet's block size (row group size) is larger than " +
          s"mapred.min.split.size/mapreduce.input.fileinputformat.split.minsize. Setting " +
          s"mapred.min.split.size and mapreduce.input.fileinputformat.split.minsize to " +
          s"$parquetBlockSize."
      logDebug(message)
      conf.set("mapred.min.split.size", parquetBlockSize.toString)
      conf.set("mapreduce.input.fileinputformat.split.minsize",
               parquetBlockSize.toString)
    }
  }

  /** This closure sets various Parquet configurations at both driver side and executor side. */
  private[parquet] def initializeLocalJobFunc(
      requiredColumns: Array[String],
      filters: Array[Filter],
      dataSchema: StructType,
      parquetBlockSize: Long,
      useMetadataCache: Boolean,
      parquetFilterPushDown: Boolean,
      assumeBinaryIsString: Boolean,
      assumeInt96IsTimestamp: Boolean)(job: Job): Unit = {
    val conf = job.getConfiguration
    conf.set(ParquetInputFormat.READ_SUPPORT_CLASS,
             classOf[CatalystReadSupport].getName)

    // Try to push down filters when filter push-down is enabled.
    if (parquetFilterPushDown) {
      filters
      // Collects all converted Parquet filter predicates. Notice that not all predicates can be
      // converted (`ParquetFilters.createFilter` returns an `Option`). That's why a `flatMap`
      // is used here.
        .flatMap(ParquetFilters.createFilter(dataSchema, _))
        .reduceOption(FilterApi.and)
        .foreach(ParquetInputFormat.setFilterPredicate(conf, _))
    }

    conf.set(CatalystReadSupport.SPARK_ROW_REQUESTED_SCHEMA, {
      val requestedSchema = StructType(requiredColumns.map(dataSchema(_)))
      CatalystSchemaConverter.checkFieldNames(requestedSchema).json
    })

    conf.set(CatalystWriteSupport.SPARK_ROW_SCHEMA,
             CatalystSchemaConverter.checkFieldNames(dataSchema).json)

    // Tell FilteringParquetRowInputFormat whether it's okay to cache Parquet and FS metadata
    conf.setBoolean(SQLConf.PARQUET_CACHE_METADATA.key, useMetadataCache)

    // Sets flags for `CatalystSchemaConverter`
    conf.setBoolean(SQLConf.PARQUET_BINARY_AS_STRING.key, assumeBinaryIsString)
    conf.setBoolean(SQLConf.PARQUET_INT96_AS_TIMESTAMP.key,
                    assumeInt96IsTimestamp)

    overrideMinSplitSize(parquetBlockSize, conf)
  }

  /** This closure sets input paths at the driver side. */
  private[parquet] def initializeDriverSideJobFunc(
      inputFiles: Array[FileStatus],
      parquetBlockSize: Long)(job: Job): Unit = {
    // We side the input paths at the driver side.
    logInfo(
        s"Reading Parquet file(s) from ${inputFiles.map(_.getPath).mkString(", ")}")
    if (inputFiles.nonEmpty) {
      FileInputFormat.setInputPaths(job, inputFiles.map(_.getPath): _*)
    }

    overrideMinSplitSize(parquetBlockSize, job.getConfiguration)
  }

  private[parquet] def readSchema(
      footers: Seq[Footer],
      sqlContext: SQLContext): Option[StructType] = {

    def parseParquetSchema(schema: MessageType): StructType = {
      val converter = new CatalystSchemaConverter(
          sqlContext.conf.isParquetBinaryAsString,
          sqlContext.conf.isParquetBinaryAsString,
          sqlContext.conf.writeLegacyParquetFormat)

      converter.convert(schema)
    }

    val seen = mutable.HashSet[String]()
    val finalSchemas: Seq[StructType] = footers.flatMap { footer =>
      val metadata = footer.getParquetMetadata.getFileMetaData
      val serializedSchema = metadata.getKeyValueMetaData.asScala.toMap
        .get(CatalystReadSupport.SPARK_METADATA_KEY)
      if (serializedSchema.isEmpty) {
        // Falls back to Parquet schema if no Spark SQL schema found.
        Some(parseParquetSchema(metadata.getSchema))
      } else if (!seen.contains(serializedSchema.get)) {
        seen += serializedSchema.get

        // Don't throw even if we failed to parse the serialized Spark schema. Just fallback to
        // whatever is available.
        Some(Try(DataType.fromJson(serializedSchema.get)).recover {
          case _: Throwable =>
            logInfo(
                s"Serialized Spark schema in Parquet key-value metadata is not in JSON format, " +
                  "falling back to the deprecated DataType.fromCaseClassString parser.")
            LegacyTypeStringParser.parse(serializedSchema.get)
        }.recover {
          case cause: Throwable =>
            logWarning(
                s"""Failed to parse serialized Spark schema in Parquet key-value metadata:
                 |\t$serializedSchema
               """.stripMargin,
                cause)
        }.map(_.asInstanceOf[StructType]).getOrElse {
          // Falls back to Parquet schema if Spark SQL schema can't be parsed.
          parseParquetSchema(metadata.getSchema)
        })
      } else {
        None
      }
    }

    finalSchemas.reduceOption { (left, right) =>
      try left.merge(right) catch {
        case e: Throwable =>
          throw new SparkException(
              s"Failed to merge incompatible schemas $left and $right",
              e)
      }
    }
  }

  /**
    * Reconciles Hive Metastore case insensitivity issue and data type conflicts between Metastore
    * schema and Parquet schema.
    *
    * Hive doesn't retain case information, while Parquet is case sensitive. On the other hand, the
    * schema read from Parquet files may be incomplete (e.g. older versions of Parquet doesn't
    * distinguish binary and string).  This method generates a correct schema by merging Metastore
    * schema data types and Parquet schema field names.
    */
  private[sql] def mergeMetastoreParquetSchema(
      metastoreSchema: StructType,
      parquetSchema: StructType): StructType = {
    def schemaConflictMessage: String =
      s"""Converting Hive Metastore Parquet, but detected conflicting schemas. Metastore schema:
         |${metastoreSchema.prettyJson}
         |
         |Parquet schema:
         |${parquetSchema.prettyJson}
       """.stripMargin

    val mergedParquetSchema =
      mergeMissingNullableFields(metastoreSchema, parquetSchema)

    assert(metastoreSchema.size <= mergedParquetSchema.size,
           schemaConflictMessage)

    val ordinalMap = metastoreSchema.zipWithIndex.map {
      case (field, index) => field.name.toLowerCase -> index
    }.toMap

    val reorderedParquetSchema = mergedParquetSchema.sortBy(f =>
          ordinalMap.getOrElse(f.name.toLowerCase, metastoreSchema.size + 1))

    StructType(metastoreSchema.zip(reorderedParquetSchema).map {
      // Uses Parquet field names but retains Metastore data types.
      case (mSchema, pSchema)
          if mSchema.name.toLowerCase == pSchema.name.toLowerCase =>
        mSchema.copy(name = pSchema.name)
      case _ =>
        throw new SparkException(schemaConflictMessage)
    })
  }

  /**
    * Returns the original schema from the Parquet file with any missing nullable fields from the
    * Hive Metastore schema merged in.
    *
    * When constructing a DataFrame from a collection of structured data, the resulting object has
    * a schema corresponding to the union of the fields present in each element of the collection.
    * Spark SQL simply assigns a null value to any field that isn't present for a particular row.
    * In some cases, it is possible that a given table partition stored as a Parquet file doesn't
    * contain a particular nullable field in its schema despite that field being present in the
    * table schema obtained from the Hive Metastore. This method returns a schema representing the
    * Parquet file schema along with any additional nullable fields from the Metastore schema
    * merged in.
    */
  private[parquet] def mergeMissingNullableFields(
      metastoreSchema: StructType,
      parquetSchema: StructType): StructType = {
    val fieldMap = metastoreSchema.map(f => f.name.toLowerCase -> f).toMap
    val missingFields = metastoreSchema
      .map(_.name.toLowerCase)
      .diff(parquetSchema.map(_.name.toLowerCase))
      .map(fieldMap(_))
      .filter(_.nullable)
    StructType(parquetSchema ++ missingFields)
  }

  /**
    * Figures out a merged Parquet schema with a distributed Spark job.
    *
    * Note that locality is not taken into consideration here because:
    *
    *  1. For a single Parquet part-file, in most cases the footer only resides in the last block of
    *     that file.  Thus we only need to retrieve the location of the last block.  However, Hadoop
    *     `FileSystem` only provides API to retrieve locations of all blocks, which can be
    *     potentially expensive.
    *
    *  2. This optimization is mainly useful for S3, where file metadata operations can be pretty
    *     slow.  And basically locality is not available when using S3 (you can't run computation on
    *     S3 nodes).
    */
  def mergeSchemasInParallel(filesToTouch: Seq[FileStatus],
                             sqlContext: SQLContext): Option[StructType] = {
    val assumeBinaryIsString = sqlContext.conf.isParquetBinaryAsString
    val assumeInt96IsTimestamp = sqlContext.conf.isParquetINT96AsTimestamp
    val writeLegacyParquetFormat = sqlContext.conf.writeLegacyParquetFormat
    val serializedConf = new SerializableConfiguration(
        sqlContext.sparkContext.hadoopConfiguration)

    // !! HACK ALERT !!
    //
    // Parquet requires `FileStatus`es to read footers.  Here we try to send cached `FileStatus`es
    // to executor side to avoid fetching them again.  However, `FileStatus` is not `Serializable`
    // but only `Writable`.  What makes it worth, for some reason, `FileStatus` doesn't play well
    // with `SerializableWritable[T]` and always causes a weird `IllegalStateException`.  These
    // facts virtually prevents us to serialize `FileStatus`es.
    //
    // Since Parquet only relies on path and length information of those `FileStatus`es to read
    // footers, here we just extract them (which can be easily serialized), send them to executor
    // side, and resemble fake `FileStatus`es there.
    val partialFileStatusInfo =
      filesToTouch.map(f => (f.getPath.toString, f.getLen))

    // Issues a Spark job to read Parquet schema in parallel.
    val partiallyMergedSchemas = sqlContext.sparkContext
      .parallelize(partialFileStatusInfo)
      .mapPartitions { iterator =>
        // Resembles fake `FileStatus`es with serialized path and length information.
        val fakeFileStatuses = iterator.map {
          case (path, length) =>
            new FileStatus(length,
                           false,
                           0,
                           0,
                           0,
                           0,
                           null,
                           null,
                           null,
                           new Path(path))
        }.toSeq

        // Skips row group information since we only need the schema
        val skipRowGroups = true

        // Reads footers in multi-threaded manner within each task
        val footers = ParquetFileReader
          .readAllFootersInParallel(serializedConf.value,
                                    fakeFileStatuses.asJava,
                                    skipRowGroups)
          .asScala

        // Converter used to convert Parquet `MessageType` to Spark SQL `StructType`
        val converter = new CatalystSchemaConverter(
            assumeBinaryIsString = assumeBinaryIsString,
            assumeInt96IsTimestamp = assumeInt96IsTimestamp,
            writeLegacyParquetFormat = writeLegacyParquetFormat)

        if (footers.isEmpty) {
          Iterator.empty
        } else {
          var mergedSchema =
            ParquetRelation.readSchemaFromFooter(footers.head, converter)
          footers.tail.foreach { footer =>
            val schema =
              ParquetRelation.readSchemaFromFooter(footer, converter)
            try {
              mergedSchema = mergedSchema.merge(schema)
            } catch {
              case cause: SparkException =>
                throw new SparkException(
                    s"Failed merging schema of file ${footer.getFile}:\n${schema.treeString}",
                    cause)
            }
          }
          Iterator.single(mergedSchema)
        }
      }
      .collect()

    if (partiallyMergedSchemas.isEmpty) {
      None
    } else {
      var finalSchema = partiallyMergedSchemas.head
      partiallyMergedSchemas.tail.foreach { schema =>
        try {
          finalSchema = finalSchema.merge(schema)
        } catch {
          case cause: SparkException =>
            throw new SparkException(
                s"Failed merging schema:\n${schema.treeString}",
                cause)
        }
      }
      Some(finalSchema)
    }
  }

  /**
    * Reads Spark SQL schema from a Parquet footer.  If a valid serialized Spark SQL schema string
    * can be found in the file metadata, returns the deserialized [[StructType]], otherwise, returns
    * a [[StructType]] converted from the [[MessageType]] stored in this footer.
    */
  def readSchemaFromFooter(footer: Footer,
                           converter: CatalystSchemaConverter): StructType = {
    val fileMetaData = footer.getParquetMetadata.getFileMetaData
    fileMetaData.getKeyValueMetaData.asScala.toMap
      .get(CatalystReadSupport.SPARK_METADATA_KEY)
      .flatMap(deserializeSchemaString)
      .getOrElse(converter.convert(fileMetaData.getSchema))
  }

  private def deserializeSchemaString(
      schemaString: String): Option[StructType] = {
    // Tries to deserialize the schema string as JSON first, then falls back to the case class
    // string parser (data generated by older versions of Spark SQL uses this format).
    Try(DataType.fromJson(schemaString).asInstanceOf[StructType]).recover {
      case _: Throwable =>
        logInfo(
            s"Serialized Spark schema in Parquet key-value metadata is not in JSON format, " +
              "falling back to the deprecated DataType.fromCaseClassString parser.")
        LegacyTypeStringParser.parse(schemaString).asInstanceOf[StructType]
    }.recoverWith {
      case cause: Throwable =>
        logWarning("Failed to parse and ignored serialized Spark schema in " +
                     s"Parquet key-value metadata:\n\t$schemaString",
                   cause)
        Failure(cause)
    }.toOption
  }

  // JUL loggers must be held by a strong reference, otherwise they may get destroyed by GC.
  // However, the root JUL logger used by Parquet isn't properly referenced.  Here we keep
  // references to loggers in both parquet-mr <= 1.6 and >= 1.7
  val apacheParquetLogger: JLogger =
    JLogger.getLogger(classOf[ApacheParquetLog].getPackage.getName)
  val parquetLogger: JLogger = JLogger.getLogger("parquet")

  // Parquet initializes its own JUL logger in a static block which always prints to stdout.  Here
  // we redirect the JUL logger via SLF4J JUL bridge handler.
  val redirectParquetLogsViaSLF4J: Unit = {
    def redirect(logger: JLogger): Unit = {
      logger.getHandlers.foreach(logger.removeHandler)
      logger.setUseParentHandlers(false)
      logger.addHandler(new SLF4JBridgeHandler)
    }

    // For parquet-mr 1.7.0 and above versions, which are under `org.apache.parquet` namespace.
    // scalastyle:off classforname
    Class.forName(classOf[ApacheParquetLog].getName)
    // scalastyle:on classforname
    redirect(JLogger.getLogger(classOf[ApacheParquetLog].getPackage.getName))

    // For parquet-mr 1.6.0 and lower versions bundled with Hive, which are under `parquet`
    // namespace.
    try {
      // scalastyle:off classforname
      Class.forName("parquet.Log")
      // scalastyle:on classforname
      redirect(JLogger.getLogger("parquet"))
    } catch {
      case _: Throwable =>
      // SPARK-9974: com.twitter:parquet-hadoop-bundle:1.6.0 is not packaged into the assembly jar
      // when Spark is built with SBT. So `parquet.Log` may not be found.  This try/catch block
      // should be removed after this issue is fixed.
    }
  }

  // The parquet compression short names
  val shortParquetCompressionCodecNames = Map(
      "none" -> CompressionCodecName.UNCOMPRESSED,
      "uncompressed" -> CompressionCodecName.UNCOMPRESSED,
      "snappy" -> CompressionCodecName.SNAPPY,
      "gzip" -> CompressionCodecName.GZIP,
      "lzo" -> CompressionCodecName.LZO)
}
