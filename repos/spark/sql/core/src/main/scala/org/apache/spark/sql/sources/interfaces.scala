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

package org.apache.spark.sql.sources

import scala.collection.mutable
import scala.util.Try

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileStatus, FileSystem, Path}
import org.apache.hadoop.mapred.{FileInputFormat, JobConf}
import org.apache.hadoop.mapreduce.{Job, TaskAttemptContext}

import org.apache.spark.SparkContext
import org.apache.spark.annotation.{DeveloperApi, Experimental}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.internal.Logging
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._
import org.apache.spark.sql.catalyst.{expressions, CatalystTypeConverters, InternalRow}
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.execution.FileRelation
import org.apache.spark.sql.execution.datasources._
import org.apache.spark.sql.execution.streaming.{Sink, Source}
import org.apache.spark.sql.types.{StringType, StructType}
import org.apache.spark.util.SerializableConfiguration
import org.apache.spark.util.collection.BitSet

/**
  * ::DeveloperApi::
  * Data sources should implement this trait so that they can register an alias to their data source.
  * This allows users to give the data source alias as the format type over the fully qualified
  * class name.
  *
  * A new instance of this class will be instantiated each time a DDL call is made.
  *
  * @since 1.5.0
  */
@DeveloperApi
trait DataSourceRegister

  /**
    * The string that represents the format that this data source provider uses. This is
    * overridden by children to provide a nice alias for the data source. For example:
    *
    * {{{
    *   override def shortName(): String = "parquet"
    * }}}
    *
    * @since 1.5.0
    */
  def shortName(): String

/**
  * ::DeveloperApi::
  * Implemented by objects that produce relations for a specific kind of data source.  When
  * Spark SQL is given a DDL operation with a USING clause specified (to specify the implemented
  * RelationProvider), this interface is used to pass in the parameters specified by a user.
  *
  * Users may specify the fully qualified class name of a given data source.  When that class is
  * not found Spark SQL will append the class name `DefaultSource` to the path, allowing for
  * less verbose invocation.  For example, 'org.apache.spark.sql.json' would resolve to the
  * data source 'org.apache.spark.sql.json.DefaultSource'
  *
  * A new instance of this class will be instantiated each time a DDL call is made.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait RelationProvider

  /**
    * Returns a new base relation with the given parameters.
    * Note: the parameters' keywords are case insensitive and this insensitivity is enforced
    * by the Map that is passed to the function.
    */
  def createRelation(
      sqlContext: SQLContext, parameters: Map[String, String]): BaseRelation

/**
  * ::DeveloperApi::
  * Implemented by objects that produce relations for a specific kind of data source
  * with a given schema.  When Spark SQL is given a DDL operation with a USING clause specified (
  * to specify the implemented SchemaRelationProvider) and a user defined schema, this interface
  * is used to pass in the parameters specified by a user.
  *
  * Users may specify the fully qualified class name of a given data source.  When that class is
  * not found Spark SQL will append the class name `DefaultSource` to the path, allowing for
  * less verbose invocation.  For example, 'org.apache.spark.sql.json' would resolve to the
  * data source 'org.apache.spark.sql.json.DefaultSource'
  *
  * A new instance of this class will be instantiated each time a DDL call is made.
  *
  * The difference between a [[RelationProvider]] and a [[SchemaRelationProvider]] is that
  * users need to provide a schema when using a [[SchemaRelationProvider]].
  * A relation provider can inherits both [[RelationProvider]] and [[SchemaRelationProvider]]
  * if it can support both schema inference and user-specified schemas.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait SchemaRelationProvider

  /**
    * Returns a new base relation with the given parameters and user defined schema.
    * Note: the parameters' keywords are case insensitive and this insensitivity is enforced
    * by the Map that is passed to the function.
    */
  def createRelation(sqlContext: SQLContext,
                     parameters: Map[String, String],
                     schema: StructType): BaseRelation

/**
  * Implemented by objects that can produce a streaming [[Source]] for a specific format or system.
  */
trait StreamSourceProvider
  def createSource(sqlContext: SQLContext,
                   schema: Option[StructType],
                   providerName: String,
                   parameters: Map[String, String]): Source

/**
  * Implemented by objects that can produce a streaming [[Sink]] for a specific format or system.
  */
trait StreamSinkProvider
  def createSink(sqlContext: SQLContext,
                 parameters: Map[String, String],
                 partitionColumns: Seq[String]): Sink

/**
  * @since 1.3.0
  */
@DeveloperApi
trait CreatableRelationProvider

  /**
    * Creates a relation with the given parameters based on the contents of the given
    * DataFrame. The mode specifies the expected behavior of createRelation when
    * data already exists.
    * Right now, there are three modes, Append, Overwrite, and ErrorIfExists.
    * Append mode means that when saving a DataFrame to a data source, if data already exists,
    * contents of the DataFrame are expected to be appended to existing data.
    * Overwrite mode means that when saving a DataFrame to a data source, if data already exists,
    * existing data is expected to be overwritten by the contents of the DataFrame.
    * ErrorIfExists mode means that when saving a DataFrame to a data source,
    * if data already exists, an exception is expected to be thrown.
    *
    * @since 1.3.0
    */
  def createRelation(sqlContext: SQLContext,
                     mode: SaveMode,
                     parameters: Map[String, String],
                     data: DataFrame): BaseRelation

/**
  * ::DeveloperApi::
  * Represents a collection of tuples with a known schema. Classes that extend BaseRelation must
  * be able to produce the schema of their data in the form of a [[StructType]]. Concrete
  * implementation should inherit from one of the descendant `Scan` classes, which define various
  * abstract methods for execution.
  *
  * BaseRelations must also define an equality function that only returns true when the two
  * instances will return the same data. This equality function is used when determining when
  * it is safe to substitute cached results for a given relation.
  *
  * @since 1.3.0
  */
@DeveloperApi
abstract class BaseRelation
  def sqlContext: SQLContext
  def schema: StructType

  /**
    * Returns an estimated size of this relation in bytes. This information is used by the planner
    * to decide when it is safe to broadcast a relation and can be overridden by sources that
    * know the size ahead of time. By default, the system will assume that tables are too
    * large to broadcast. This method will be called multiple times during query planning
    * and thus should not perform expensive operations for each invocation.
    *
    * Note that it is always better to overestimate size than underestimate, because underestimation
    * could lead to execution plans that are suboptimal (i.e. broadcasting a very large table).
    *
    * @since 1.3.0
    */
  def sizeInBytes: Long = sqlContext.conf.defaultSizeInBytes

  /**
    * Whether does it need to convert the objects in Row to internal representation, for example:
    *  java.lang.String -> UTF8String
    *  java.lang.Decimal -> Decimal
    *
    * If `needConversion` is `false`, buildScan() should return an [[RDD]] of [[InternalRow]]
    *
    * Note: The internal representation is not stable across releases and thus data sources outside
    * of Spark SQL should leave this as true.
    *
    * @since 1.4.0
    */
  def needConversion: Boolean = true

  /**
    * Returns the list of [[Filter]]s that this datasource may not be able to handle.
    * These returned [[Filter]]s will be evaluated by Spark SQL after data is output by a scan.
    * By default, this function will return all filters, as it is always safe to
    * double evaluate a [[Filter]]. However, specific implementations can override this function to
    * avoid double filtering when they are capable of processing a filter internally.
    *
    * @since 1.6.0
    */
  def unhandledFilters(filters: Array[Filter]): Array[Filter] = filters

/**
  * ::DeveloperApi::
  * A BaseRelation that can produce all of its tuples as an RDD of Row objects.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait TableScan
  def buildScan(): RDD[Row]

/**
  * ::DeveloperApi::
  * A BaseRelation that can eliminate unneeded columns before producing an RDD
  * containing all of its tuples as Row objects.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait PrunedScan
  def buildScan(requiredColumns: Array[String]): RDD[Row]

/**
  * ::DeveloperApi::
  * A BaseRelation that can eliminate unneeded columns and filter using selected
  * predicates before producing an RDD containing all matching tuples as Row objects.
  *
  * The actual filter should be the conjunction of all `filters`,
  * i.e. they should be "and" together.
  *
  * The pushed down filters are currently purely an optimization as they will all be evaluated
  * again.  This means it is safe to use them with methods that produce false positives such
  * as filtering partitions based on a bloom filter.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait PrunedFilteredScan
  def buildScan(
      requiredColumns: Array[String], filters: Array[Filter]): RDD[Row]

/**
  * ::DeveloperApi::
  * A BaseRelation that can be used to insert data into it through the insert method.
  * If overwrite in insert method is true, the old data in the relation should be overwritten with
  * the new data. If overwrite in insert method is false, the new data should be appended.
  *
  * InsertableRelation has the following three assumptions.
  * 1. It assumes that the data (Rows in the DataFrame) provided to the insert method
  * exactly matches the ordinal of fields in the schema of the BaseRelation.
  * 2. It assumes that the schema of this relation will not be changed.
  * Even if the insert method updates the schema (e.g. a relation of JSON or Parquet data may have a
  * schema update after an insert operation), the new schema will not be used.
  * 3. It assumes that fields of the data provided in the insert method are nullable.
  * If a data source needs to check the actual nullability of a field, it needs to do it in the
  * insert method.
  *
  * @since 1.3.0
  */
@DeveloperApi
trait InsertableRelation
  def insert(data: DataFrame, overwrite: Boolean): Unit

/**
  * ::Experimental::
  * An interface for experimenting with a more direct connection to the query planner.  Compared to
  * [[PrunedFilteredScan]], this operator receives the raw expressions from the
  * [[org.apache.spark.sql.catalyst.plans.logical.LogicalPlan]].  Unlike the other APIs this
  * interface is NOT designed to be binary compatible across releases and thus should only be used
  * for experimentation.
  *
  * @since 1.3.0
  */
@Experimental
trait CatalystScan
  def buildScan(
      requiredColumns: Seq[Attribute], filters: Seq[Expression]): RDD[Row]

/**
  * ::Experimental::
  * A factory that produces [[OutputWriter]]s.  A new [[OutputWriterFactory]] is created on driver
  * side for each write job issued when writing to a [[HadoopFsRelation]], and then gets serialized
  * to executor side to create actual [[OutputWriter]]s on the fly.
  *
  * @since 1.4.0
  */
@Experimental
abstract class OutputWriterFactory extends Serializable

  /**
    * When writing to a [[HadoopFsRelation]], this method gets called by each task on executor side
    * to instantiate new [[OutputWriter]]s.
    *
    * @param path Path of the file to which this [[OutputWriter]] is supposed to write.  Note that
    *        this may not point to the final output file.  For example, `FileOutputFormat` writes to
    *        temporary directories and then merge written files back to the final destination.  In
    *        this case, `path` points to a temporary output file under the temporary directory.
    * @param dataSchema Schema of the rows to be written. Partition columns are not included in the
    *        schema if the relation being written is partitioned.
    * @param context The Hadoop MapReduce task context.
    * @since 1.4.0
    */
  private[sql] def newInstance(
      path: String,
      bucketId: Option[Int], // TODO: This doesn't belong here...
      dataSchema: StructType,
      context: TaskAttemptContext): OutputWriter

/**
  * ::Experimental::
  * [[OutputWriter]] is used together with [[HadoopFsRelation]] for persisting rows to the
  * underlying file system.  Subclasses of [[OutputWriter]] must provide a zero-argument constructor.
  * An [[OutputWriter]] instance is created and initialized when a new output file is opened on
  * executor side.  This instance is used to persist rows to this single output file.
  *
  * @since 1.4.0
  */
@Experimental
abstract class OutputWriter

  /**
    * Persists a single row.  Invoked on the executor side.  When writing to dynamically partitioned
    * tables, dynamic partition columns are not included in rows to be written.
    *
    * @since 1.4.0
    */
  def write(row: Row): Unit

  /**
    * Closes the [[OutputWriter]]. Invoked on the executor side after all rows are persisted, before
    * the task output is committed.
    *
    * @since 1.4.0
    */
  def close(): Unit

  private var converter: InternalRow => Row = _

  protected[sql] def initConverter(dataSchema: StructType) =
    converter = CatalystTypeConverters
      .createToScalaConverter(dataSchema)
      .asInstanceOf[InternalRow => Row]

  protected[sql] def writeInternal(row: InternalRow): Unit =
    write(converter(row))

/**
  * Acts as a container for all of the metadata required to read from a datasource. All discovery,
  * resolution and merging logic for schemas and partitions has been removed.
  *
  * @param location A [[FileCatalog]] that can enumerate the locations of all the files that comprise
  *                 this relation.
  * @param partitionSchema The schmea of the columns (if any) that are used to partition the relation
  * @param dataSchema The schema of any remaining columns.  Note that if any partition columns are
  *                   present in the actual data files as well, they are removed.
  * @param bucketSpec Describes the bucketing (hash-partitioning of the files by some column values).
  * @param fileFormat A file format that can be used to read and write the data in files.
  * @param options Configuration used when reading / writing data.
  */
case class HadoopFsRelation(sqlContext: SQLContext,
                            location: FileCatalog,
                            partitionSchema: StructType,
                            dataSchema: StructType,
                            bucketSpec: Option[BucketSpec],
                            fileFormat: FileFormat,
                            options: Map[String, String])
    extends BaseRelation with FileRelation

  val schema: StructType =
    val dataSchemaColumnNames = dataSchema.map(_.name.toLowerCase).toSet
    StructType(
        dataSchema ++ partitionSchema.filterNot  column =>
      dataSchemaColumnNames.contains(column.name.toLowerCase)
    )

  def partitionSchemaOption: Option[StructType] =
    if (partitionSchema.isEmpty) None else Some(partitionSchema)
  def partitionSpec: PartitionSpec = location.partitionSpec()

  def refresh(): Unit = location.refresh()

  override def toString: String =
    s"$fileFormat part: ${partitionSchema.simpleString}, data: ${dataSchema.simpleString}"

  /** Returns the list of files that will be read when scanning this relation. */
  override def inputFiles: Array[String] =
    location.allFiles().map(_.getPath.toUri.toString).toArray

  override def sizeInBytes: Long = location.allFiles().map(_.getLen).sum

/**
  * Used to read and write data stored in files to/from the [[InternalRow]] format.
  */
trait FileFormat

  /**
    * When possible, this method should return the schema of the given `files`.  When the format
    * does not support inference, or no valid files are given should return None.  In these cases
    * Spark will require that user specify the schema manually.
    */
  def inferSchema(sqlContext: SQLContext,
                  options: Map[String, String],
                  files: Seq[FileStatus]): Option[StructType]

  /**
    * Prepares a write job and returns an [[OutputWriterFactory]].  Client side job preparation can
    * be put here.  For example, user defined output committer can be configured here
    * by setting the output committer class in the conf of spark.sql.sources.outputCommitterClass.
    */
  def prepareWrite(sqlContext: SQLContext,
                   job: Job,
                   options: Map[String, String],
                   dataSchema: StructType): OutputWriterFactory

  def buildInternalScan(sqlContext: SQLContext,
                        dataSchema: StructType,
                        requiredColumns: Array[String],
                        filters: Array[Filter],
                        bucketSet: Option[BitSet],
                        inputFiles: Seq[FileStatus],
                        broadcastedConf: Broadcast[SerializableConfiguration],
                        options: Map[String, String]): RDD[InternalRow]

  /**
    * Returns a function that can be used to read a single file in as an Iterator of InternalRow.
    *
    * @param partitionSchema The schema of the partition column row that will be present in each
    *                        PartitionedFile.  These columns should be prepended to the rows that
    *                        are produced by the iterator.
    * @param dataSchema The schema of the data that should be output for each row.  This may be a
    *                   subset of the columns that are present in the file if  column pruning has
    *                   occurred.
    * @param filters A set of filters than can optionally be used to reduce the number of rows output
    * @param options A set of string -> string configuration options.
    * @return
    */
  def buildReader(sqlContext: SQLContext,
                  partitionSchema: StructType,
                  dataSchema: StructType,
                  filters: Seq[Filter],
                  options: Map[String, String])
    : PartitionedFile => Iterator[InternalRow] =
    // TODO: Remove this default implementation when the other formats have been ported
    // Until then we guard in [[FileSourceStrategy]] to only call this method on supported formats.
    throw new UnsupportedOperationException(
        s"buildReader is not supported for $this")

/**
  * A collection of data files from a partitioned relation, along with the partition values in the
  * form of an [[InternalRow]].
  */
case class Partition(values: InternalRow, files: Seq[FileStatus])

/**
  * An interface for objects capable of enumerating the files that comprise a relation as well
  * as the partitioning characteristics of those files.
  */
trait FileCatalog
  def paths: Seq[Path]

  def partitionSpec(): PartitionSpec

  /**
    * Returns all valid files grouped into partitions when the data is partitioned. If the data is
    * unpartitioned, this will return a single partition with not partition values.
    *
    * @param filters the filters used to prune which partitions are returned.  These filters must
    *                only refer to partition columns and this method will only return files
    *                where these predicates are guaranteed to evaluate to `true`.  Thus, these
    *                filters will not need to be evaluated again on the returned data.
    */
  def listFiles(filters: Seq[Expression]): Seq[Partition]

  def allFiles(): Seq[FileStatus]

  def getStatus(path: Path): Array[FileStatus]

  def refresh(): Unit

/**
  * A file catalog that caches metadata gathered by scanning all the files present in `paths`
  * recursively.
  *
  * @param parameters as set of options to control discovery
  * @param paths a list of paths to scan
  * @param partitionSchema an optional partition schema that will be use to provide types for the
  *                        discovered partitions
  */
class HDFSFileCatalog(val sqlContext: SQLContext,
                      val parameters: Map[String, String],
                      val paths: Seq[Path],
                      val partitionSchema: Option[StructType])
    extends FileCatalog with Logging

  private val hadoopConf = new Configuration(
      sqlContext.sparkContext.hadoopConfiguration)

  var leafFiles = mutable.LinkedHashMap.empty[Path, FileStatus]
  var leafDirToChildrenFiles = mutable.Map.empty[Path, Array[FileStatus]]
  var cachedPartitionSpec: PartitionSpec = _

  def partitionSpec(): PartitionSpec =
    if (cachedPartitionSpec == null)
      cachedPartitionSpec = inferPartitioning(partitionSchema)

    cachedPartitionSpec

  refresh()

  override def listFiles(filters: Seq[Expression]): Seq[Partition] =
    if (partitionSpec().partitionColumns.isEmpty)
      Partition(InternalRow.empty, allFiles()) :: Nil
    else
      prunePartitions(filters, partitionSpec()).map
        case PartitionDirectory(values, path) =>
          Partition(values, getStatus(path))

  protected def prunePartitions(
      predicates: Seq[Expression],
      partitionSpec: PartitionSpec): Seq[PartitionDirectory] =
    val PartitionSpec(partitionColumns, partitions) = partitionSpec
    val partitionColumnNames = partitionColumns.map(_.name).toSet
    val partitionPruningPredicates = predicates.filter
      _.references.map(_.name).toSet.subsetOf(partitionColumnNames)

    if (partitionPruningPredicates.nonEmpty)
      val predicate = partitionPruningPredicates
        .reduceOption(expressions.And)
        .getOrElse(Literal(true))

      val boundPredicate = InterpretedPredicate.create(
          predicate.transform
        case a: AttributeReference =>
          val index = partitionColumns.indexWhere(a.name == _.name)
          BoundReference(
              index, partitionColumns(index).dataType, nullable = true)
      )

      val selected = partitions.filter
        case PartitionDirectory(values, _) => boundPredicate(values)
      logInfo
        val total = partitions.length
        val selectedSize = selected.length
        val percentPruned = (1 - selectedSize.toDouble / total.toDouble) * 100
        s"Selected $selectedSize partitions out of $total, pruned $percentPruned% partitions."

      selected
    else
      partitions

  def allFiles(): Seq[FileStatus] = leafFiles.values.toSeq

  def getStatus(path: Path): Array[FileStatus] = leafDirToChildrenFiles(path)

  private def listLeafFiles(
      paths: Seq[Path]): mutable.LinkedHashSet[FileStatus] =
    if (paths.length >= sqlContext.conf.parallelPartitionDiscoveryThreshold)
      HadoopFsRelation.listLeafFilesInParallel(
          paths, hadoopConf, sqlContext.sparkContext)
    else
      val statuses = paths.flatMap  path =>
        val fs = path.getFileSystem(hadoopConf)
        logInfo(s"Listing $path on driver")
        // Dummy jobconf to get to the pathFilter defined in configuration
        val jobConf = new JobConf(hadoopConf, this.getClass())
        val pathFilter = FileInputFormat.getInputPathFilter(jobConf)
        if (pathFilter != null)
          Try(fs.listStatus(path, pathFilter)).getOrElse(Array.empty)
        else
          Try(fs.listStatus(path)).getOrElse(Array.empty)
      .filterNot  status =>
        val name = status.getPath.getName
        HadoopFsRelation.shouldFilterOut(name)

      val (dirs, files) = statuses.partition(_.isDirectory)

      // It uses [[LinkedHashSet]] since the order of files can affect the results. (SPARK-11500)
      if (dirs.isEmpty)
        mutable.LinkedHashSet(files: _*)
      else
        mutable.LinkedHashSet(files: _*) ++ listLeafFiles(dirs.map(_.getPath))

  def inferPartitioning(schema: Option[StructType]): PartitionSpec =
    // We use leaf dirs containing data files to discover the schema.
    val leafDirs = leafDirToChildrenFiles.keys.toSeq
    schema match
      case Some(userProvidedSchema) if userProvidedSchema.nonEmpty =>
        val spec = PartitioningUtils.parsePartitions(
            leafDirs,
            PartitioningUtils.DEFAULT_PARTITION_NAME,
            typeInference = false,
            basePaths = basePaths)

        // Without auto inference, all of value in the `row` should be null or in StringType,
        // we need to cast into the data type that user specified.
        def castPartitionValuesToUserSchema(row: InternalRow) =
          InternalRow(
              (0 until row.numFields).map  i =>
            Cast(Literal.create(row.getUTF8String(i), StringType),
                 userProvidedSchema.fields(i).dataType).eval()
          : _*)

        PartitionSpec(userProvidedSchema, spec.partitions.map  part =>
          part.copy(values = castPartitionValuesToUserSchema(part.values))
        )
      case _ =>
        PartitioningUtils.parsePartitions(
            leafDirs,
            PartitioningUtils.DEFAULT_PARTITION_NAME,
            typeInference = sqlContext.conf
                .partitionColumnTypeInferenceEnabled(),
            basePaths = basePaths)

  /**
    * Contains a set of paths that are considered as the base dirs of the input datasets.
    * The partitioning discovery logic will make sure it will stop when it reaches any
    * base path. By default, the paths of the dataset provided by users will be base paths.
    * For example, if a user uses `sqlContext.read.parquet("/path/something=true/")`, the base path
    * will be `/path/something=true/`, and the returned DataFrame will not contain a column of
    * `something`. If users want to override the basePath. They can set `basePath` in the options
    * to pass the new base path to the data source.
    * For the above example, if the user-provided base path is `/path/`, the returned
    * DataFrame will have the column of `something`.
    */
  private def basePaths: Set[Path] =
    val userDefinedBasePath =
      parameters.get("basePath").map(basePath => Set(new Path(basePath)))
    userDefinedBasePath.getOrElse
      // If the user does not provide basePath, we will just use paths.
      paths.toSet
    .map  hdfsPath =>
      // Make the path qualified (consistent with listLeafFiles and listLeafFilesInParallel).
      val fs = hdfsPath.getFileSystem(hadoopConf)
      hdfsPath.makeQualified(fs.getUri, fs.getWorkingDirectory)

  def refresh(): Unit =
    val files = listLeafFiles(paths)

    leafFiles.clear()
    leafDirToChildrenFiles.clear()

    leafFiles ++= files.map(f => f.getPath -> f)
    leafDirToChildrenFiles ++= files.toArray.groupBy(_.getPath.getParent)

    cachedPartitionSpec = null

  override def equals(other: Any): Boolean = other match
    case hdfs: HDFSFileCatalog => paths.toSet == hdfs.paths.toSet
    case _ => false

  override def hashCode(): Int = paths.toSet.hashCode()

/**
  * Helper methods for gathering metadata from HDFS.
  */
private[sql] object HadoopFsRelation extends Logging

  /** Checks if we should filter out this path name. */
  def shouldFilterOut(pathName: String): Boolean =
    // TODO: We should try to filter out all files/dirs starting with "." or "_".
    // The only reason that we are not doing it now is that Parquet needs to find those
    // metadata files from leaf files returned by this methods. We should refactor
    // this logic to not mix metadata files with data files.
    pathName == "_SUCCESS" || pathName == "_temporary" ||
    pathName.startsWith(".")

  // We don't filter files/directories whose name start with "_" except "_temporary" here, as
  // specific data sources may take advantages over them (e.g. Parquet _metadata and
  // _common_metadata files). "_temporary" directories are explicitly ignored since failed
  // tasks/jobs may leave partial/corrupted data files there.  Files and directories whose name
  // start with "." are also ignored.
  def listLeafFiles(fs: FileSystem, status: FileStatus): Array[FileStatus] =
    logInfo(s"Listing ${status.getPath}")
    val name = status.getPath.getName.toLowerCase
    if (shouldFilterOut(name))
      Array.empty
    else
      // Dummy jobconf to get to the pathFilter defined in configuration
      val jobConf = new JobConf(fs.getConf, this.getClass())
      val pathFilter = FileInputFormat.getInputPathFilter(jobConf)
      val statuses =
        if (pathFilter != null)
          val (dirs, files) =
            fs.listStatus(status.getPath, pathFilter).partition(_.isDirectory)
          files ++ dirs.flatMap(dir => listLeafFiles(fs, dir))
        else
          val (dirs, files) =
            fs.listStatus(status.getPath).partition(_.isDirectory)
          files ++ dirs.flatMap(dir => listLeafFiles(fs, dir))
      statuses.filterNot(status => shouldFilterOut(status.getPath.getName))

  // `FileStatus` is Writable but not serializable.  What make it worse, somehow it doesn't play
  // well with `SerializableWritable`.  So there seems to be no way to serialize a `FileStatus`.
  // Here we use `FakeFileStatus` to extract key components of a `FileStatus` to serialize it from
  // executor side and reconstruct it on driver side.
  case class FakeFileStatus(path: String,
                            length: Long,
                            isDir: Boolean,
                            blockReplication: Short,
                            blockSize: Long,
                            modificationTime: Long,
                            accessTime: Long)

  def listLeafFilesInParallel(
      paths: Seq[Path],
      hadoopConf: Configuration,
      sparkContext: SparkContext): mutable.LinkedHashSet[FileStatus] =
    logInfo(
        s"Listing leaf files and directories in parallel under: ${paths.mkString(", ")}")

    val serializableConfiguration = new SerializableConfiguration(hadoopConf)
    val serializedPaths = paths.map(_.toString)

    val fakeStatuses = sparkContext
      .parallelize(serializedPaths)
      .map(new Path(_))
      .flatMap  path =>
        val fs = path.getFileSystem(serializableConfiguration.value)
        Try(listLeafFiles(fs, fs.getFileStatus(path))).getOrElse(Array.empty)
      .map  status =>
        FakeFileStatus(status.getPath.toString,
                       status.getLen,
                       status.isDirectory,
                       status.getReplication,
                       status.getBlockSize,
                       status.getModificationTime,
                       status.getAccessTime)
      .collect()

    val hadoopFakeStatuses = fakeStatuses.map  f =>
      new FileStatus(f.length,
                     f.isDir,
                     f.blockReplication,
                     f.blockSize,
                     f.modificationTime,
                     new Path(f.path))
    mutable.LinkedHashSet(hadoopFakeStatuses: _*)
