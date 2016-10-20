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

package org.apache.spark.sql.internal

import java.util.{NoSuchElementException, Properties}

import scala.collection.JavaConverters._
import scala.collection.immutable

import org.apache.parquet.hadoop.ParquetOutputCommitter

import org.apache.spark.internal.Logging
import org.apache.spark.sql.catalyst.CatalystConf
import org.apache.spark.sql.catalyst.parser.ParserConf
import org.apache.spark.util.Utils

////////////////////////////////////////////////////////////////////////////////////////////////////
// This file defines the configuration options for Spark SQL.
////////////////////////////////////////////////////////////////////////////////////////////////////

object SQLConf {

  private val sqlConfEntries = java.util.Collections
    .synchronizedMap(new java.util.HashMap[String, SQLConfEntry[_]]())

  /**
    * An entry contains all meta information for a configuration.
    *
    * @param key the key for the configuration
    * @param defaultValue the default value for the configuration
    * @param valueConverter how to convert a string to the value. It should throw an exception if the
    *                       string does not have the required format.
    * @param stringConverter how to convert a value to a string that the user can use it as a valid
    *                        string value. It's usually `toString`. But sometimes, a custom converter
    *                        is necessary. E.g., if T is List[String], `a, b, c` is better than
    *                        `List(a, b, c)`.
    * @param doc the document for the configuration
    * @param isPublic if this configuration is public to the user. If it's `false`, this
    *                 configuration is only used internally and we should not expose it to the user.
    * @tparam T the value type
    */
  class SQLConfEntry[T] private (val key: String,
                                 val defaultValue: Option[T],
                                 val valueConverter: String => T,
                                 val stringConverter: T => String,
                                 val doc: String,
                                 val isPublic: Boolean) {

    def defaultValueString: String =
      defaultValue.map(stringConverter).getOrElse("<undefined>")

    override def toString: String = {
      s"SQLConfEntry(key = $key, defaultValue=$defaultValueString, doc=$doc, isPublic = $isPublic)"
    }
  }

  object SQLConfEntry {

    private def apply[T](key: String,
                         defaultValue: Option[T],
                         valueConverter: String => T,
                         stringConverter: T => String,
                         doc: String,
                         isPublic: Boolean): SQLConfEntry[T] =
      sqlConfEntries.synchronized {
        if (sqlConfEntries.containsKey(key)) {
          throw new IllegalArgumentException(
            s"Duplicate SQLConfEntry. $key has been registered")
        }
        val entry = new SQLConfEntry[T](key,
                                        defaultValue,
                                        valueConverter,
                                        stringConverter,
                                        doc,
                                        isPublic)
        sqlConfEntries.put(key, entry)
        entry
      }

    def intConf(key: String,
                defaultValue: Option[Int] = None,
                doc: String = "",
                isPublic: Boolean = true): SQLConfEntry[Int] =
      SQLConfEntry(key, defaultValue, { v =>
        try {
          v.toInt
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException(
              s"$key should be int, but was $v")
        }
      }, _.toString, doc, isPublic)

    def longConf(key: String,
                 defaultValue: Option[Long] = None,
                 doc: String = "",
                 isPublic: Boolean = true): SQLConfEntry[Long] =
      SQLConfEntry(key, defaultValue, { v =>
        try {
          v.toLong
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException(
              s"$key should be long, but was $v")
        }
      }, _.toString, doc, isPublic)

    def longMemConf(key: String,
                    defaultValue: Option[Long] = None,
                    doc: String = "",
                    isPublic: Boolean = true): SQLConfEntry[Long] =
      SQLConfEntry(key, defaultValue, { v =>
        try {
          v.toLong
        } catch {
          case _: NumberFormatException =>
            try {
              Utils.byteStringAsBytes(v)
            } catch {
              case _: NumberFormatException =>
                throw new IllegalArgumentException(
                  s"$key should be long, but was $v")
            }
        }
      }, _.toString, doc, isPublic)

    def doubleConf(key: String,
                   defaultValue: Option[Double] = None,
                   doc: String = "",
                   isPublic: Boolean = true): SQLConfEntry[Double] =
      SQLConfEntry(key, defaultValue, { v =>
        try {
          v.toDouble
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException(
              s"$key should be double, but was $v")
        }
      }, _.toString, doc, isPublic)

    def booleanConf(key: String,
                    defaultValue: Option[Boolean] = None,
                    doc: String = "",
                    isPublic: Boolean = true): SQLConfEntry[Boolean] =
      SQLConfEntry(key, defaultValue, { v =>
        try {
          v.toBoolean
        } catch {
          case _: IllegalArgumentException =>
            throw new IllegalArgumentException(
              s"$key should be boolean, but was $v")
        }
      }, _.toString, doc, isPublic)

    def stringConf(key: String,
                   defaultValue: Option[String] = None,
                   doc: String = "",
                   isPublic: Boolean = true): SQLConfEntry[String] =
      SQLConfEntry(key, defaultValue, v => v, v => v, doc, isPublic)

    def enumConf[T](key: String,
                    valueConverter: String => T,
                    validValues: Set[T],
                    defaultValue: Option[T] = None,
                    doc: String = "",
                    isPublic: Boolean = true): SQLConfEntry[T] =
      SQLConfEntry(key, defaultValue, v => {
        val _v = valueConverter(v)
        if (!validValues.contains(_v)) {
          throw new IllegalArgumentException(
            s"The value of $key should be one of ${validValues.mkString(", ")}, but was $v")
        }
        _v
      }, _.toString, doc, isPublic)

    def seqConf[T](key: String,
                   valueConverter: String => T,
                   defaultValue: Option[Seq[T]] = None,
                   doc: String = "",
                   isPublic: Boolean = true): SQLConfEntry[Seq[T]] = {
      SQLConfEntry(key,
                   defaultValue,
                   _.split(",").map(valueConverter),
                   _.mkString(","),
                   doc,
                   isPublic)
    }

    def stringSeqConf(key: String,
                      defaultValue: Option[Seq[String]] = None,
                      doc: String = "",
                      isPublic: Boolean = true): SQLConfEntry[Seq[String]] = {
      seqConf(key, s => s, defaultValue, doc, isPublic)
    }
  }

  import SQLConfEntry._

  val ALLOW_MULTIPLE_CONTEXTS = booleanConf(
    "spark.sql.allowMultipleContexts",
    defaultValue = Some(true),
    doc = "When set to true, creating multiple SQLContexts/HiveContexts is allowed. " +
        "When set to false, only one SQLContext/HiveContext is allowed to be created " +
        "through the constructor (new SQLContexts/HiveContexts created through newSession " +
        "method is allowed). Please note that this conf needs to be set in Spark Conf. Once " +
        "a SQLContext/HiveContext has been created, changing the value of this conf will not " +
        "have effect.",
    isPublic = true)

  val COMPRESS_CACHED = booleanConf(
    "spark.sql.inMemoryColumnarStorage.compressed",
    defaultValue = Some(true),
    doc = "When set to true Spark SQL will automatically select a compression codec for each " +
        "column based on statistics of the data.",
    isPublic = false)

  val COLUMN_BATCH_SIZE = intConf(
    "spark.sql.inMemoryColumnarStorage.batchSize",
    defaultValue = Some(10000),
    doc = "Controls the size of batches for columnar caching.  Larger batch sizes can improve " +
        "memory utilization and compression, but risk OOMs when caching data.",
    isPublic = false)

  val IN_MEMORY_PARTITION_PRUNING = booleanConf(
    "spark.sql.inMemoryColumnarStorage.partitionPruning",
    defaultValue = Some(true),
    doc = "When true, enable partition pruning for in-memory columnar tables.",
    isPublic = false)

  val PREFER_SORTMERGEJOIN = booleanConf(
    "spark.sql.join.preferSortMergeJoin",
    defaultValue = Some(true),
    doc = "When true, prefer sort merge join over shuffle hash join.",
    isPublic = false)

  val AUTO_BROADCASTJOIN_THRESHOLD = intConf(
    "spark.sql.autoBroadcastJoinThreshold",
    defaultValue = Some(10 * 1024 * 1024),
    doc = "Configures the maximum size in bytes for a table that will be broadcast to all worker " +
        "nodes when performing a join.  By setting this value to -1 broadcasting can be disabled. " +
        "Note that currently statistics are only supported for Hive Metastore tables where the " +
        "command<code>ANALYZE TABLE &lt;tableName&gt; COMPUTE STATISTICS noscan</code> has been run.")

  val DEFAULT_SIZE_IN_BYTES = longConf(
    "spark.sql.defaultSizeInBytes",
    doc = "The default table size used in query planning. By default, it is set to a larger " +
        "value than `spark.sql.autoBroadcastJoinThreshold` to be more conservative. That is to say " +
        "by default the optimizer will not choose to broadcast a table unless it knows for sure " +
        "its size is small enough.",
    isPublic = false)

  val SHUFFLE_PARTITIONS = intConf(
    "spark.sql.shuffle.partitions",
    defaultValue = Some(200),
    doc =
      "The default number of partitions to use when shuffling data for joins or aggregations.")

  val SHUFFLE_TARGET_POSTSHUFFLE_INPUT_SIZE = longMemConf(
    "spark.sql.adaptive.shuffle.targetPostShuffleInputSize",
    defaultValue = Some(64 * 1024 * 1024),
    doc = "The target post-shuffle input size in bytes of a task.")

  val ADAPTIVE_EXECUTION_ENABLED = booleanConf(
    "spark.sql.adaptive.enabled",
    defaultValue = Some(false),
    doc = "When true, enable adaptive query execution.")

  val SHUFFLE_MIN_NUM_POSTSHUFFLE_PARTITIONS = intConf(
    "spark.sql.adaptive.minNumPostShufflePartitions",
    defaultValue = Some(-1),
    doc = "The advisory minimal number of post-shuffle partitions provided to " +
        "ExchangeCoordinator. This setting is used in our test to make sure we " +
        "have enough parallelism to expose issues that will not be exposed with a " +
        "single partition. When the value is a non-positive value, this setting will " +
        "not be provided to ExchangeCoordinator.",
    isPublic = false)

  val SUBEXPRESSION_ELIMINATION_ENABLED = booleanConf(
    "spark.sql.subexpressionElimination.enabled",
    defaultValue = Some(true),
    doc = "When true, common subexpressions will be eliminated.",
    isPublic = false)

  val CASE_SENSITIVE = booleanConf(
    "spark.sql.caseSensitive",
    defaultValue = Some(true),
    doc = "Whether the query analyzer should be case sensitive or not.")

  val PARQUET_SCHEMA_MERGING_ENABLED = booleanConf(
    "spark.sql.parquet.mergeSchema",
    defaultValue = Some(false),
    doc = "When true, the Parquet data source merges schemas collected from all data files, " +
        "otherwise the schema is picked from the summary file or a random data file " +
        "if no summary file is available.")

  val PARQUET_SCHEMA_RESPECT_SUMMARIES = booleanConf(
    "spark.sql.parquet.respectSummaryFiles",
    defaultValue = Some(false),
    doc = "When true, we make assumption that all part-files of Parquet are consistent with " +
        "summary files and we will ignore them when merging schema. Otherwise, if this is " +
        "false, which is the default, we will merge all part-files. This should be considered " +
        "as expert-only option, and shouldn't be enabled before knowing what it means exactly.")

  val PARQUET_BINARY_AS_STRING = booleanConf(
    "spark.sql.parquet.binaryAsString",
    defaultValue = Some(false),
    doc = "Some other Parquet-producing systems, in particular Impala and older versions of " +
        "Spark SQL, do not differentiate between binary data and strings when writing out the " +
        "Parquet schema. This flag tells Spark SQL to interpret binary data as a string to provide " +
        "compatibility with these systems.")

  val PARQUET_INT96_AS_TIMESTAMP = booleanConf(
    "spark.sql.parquet.int96AsTimestamp",
    defaultValue = Some(true),
    doc = "Some Parquet-producing systems, in particular Impala, store Timestamp into INT96. " +
        "Spark would also store Timestamp as INT96 because we need to avoid precision lost of the " +
        "nanoseconds field. This flag tells Spark SQL to interpret INT96 data as a timestamp to " +
        "provide compatibility with these systems.")

  val PARQUET_CACHE_METADATA = booleanConf(
    "spark.sql.parquet.cacheMetadata",
    defaultValue = Some(true),
    doc =
      "Turns on caching of Parquet schema metadata. Can speed up querying of static data.")

  val PARQUET_COMPRESSION = enumConf(
    "spark.sql.parquet.compression.codec",
    valueConverter = v => v.toLowerCase,
    validValues = Set("uncompressed", "snappy", "gzip", "lzo"),
    defaultValue = Some("gzip"),
    doc = "Sets the compression codec use when writing Parquet files. Acceptable values include: " +
        "uncompressed, snappy, gzip, lzo.")

  val PARQUET_FILTER_PUSHDOWN_ENABLED = booleanConf(
    "spark.sql.parquet.filterPushdown",
    defaultValue = Some(true),
    doc = "Enables Parquet filter push-down optimization when set to true.")

  val PARQUET_WRITE_LEGACY_FORMAT = booleanConf(
    key = "spark.sql.parquet.writeLegacyFormat",
    defaultValue = Some(false),
    doc = "Whether to follow Parquet's format specification when converting Parquet schema to " +
        "Spark SQL schema and vice versa.")

  val PARQUET_OUTPUT_COMMITTER_CLASS = stringConf(
    key = "spark.sql.parquet.output.committer.class",
    defaultValue = Some(classOf[ParquetOutputCommitter].getName),
    doc = "The output committer class used by Parquet. The specified class needs to be a " +
        "subclass of org.apache.hadoop.mapreduce.OutputCommitter.  Typically, it's also a subclass " +
        "of org.apache.parquet.hadoop.ParquetOutputCommitter.  NOTE: 1. Instead of SQLConf, this " +
        "option must be set in Hadoop Configuration.  2. This option overrides " +
        "\"spark.sql.sources.outputCommitterClass\".")

  val PARQUET_VECTORIZED_READER_ENABLED = booleanConf(
    key = "spark.sql.parquet.enableVectorizedReader",
    defaultValue = Some(true),
    doc = "Enables vectorized parquet decoding.")

  val ORC_FILTER_PUSHDOWN_ENABLED = booleanConf(
    "spark.sql.orc.filterPushdown",
    defaultValue = Some(false),
    doc = "When true, enable filter pushdown for ORC files.")

  val HIVE_VERIFY_PARTITION_PATH = booleanConf(
    "spark.sql.hive.verifyPartitionPath",
    defaultValue = Some(false),
    doc = "When true, check all the partition paths under the table\'s root directory " +
        "when reading data stored in HDFS.")

  val HIVE_METASTORE_PARTITION_PRUNING = booleanConf(
    "spark.sql.hive.metastorePartitionPruning",
    defaultValue = Some(false),
    doc = "When true, some predicates will be pushed down into the Hive metastore so that " +
        "unmatching partitions can be eliminated earlier.")

  val NATIVE_VIEW = booleanConf(
    "spark.sql.nativeView",
    defaultValue = Some(false),
    doc = "When true, CREATE VIEW will be handled by Spark SQL instead of Hive native commands.  " +
        "Note that this function is experimental and should ony be used when you are using " +
        "non-hive-compatible tables written by Spark SQL.  The SQL string used to create " +
        "view should be fully qualified, i.e. use `tbl1`.`col1` instead of `*` whenever " +
        "possible, or you may get wrong result.",
    isPublic = false)

  val CANONICAL_NATIVE_VIEW = booleanConf(
    "spark.sql.nativeView.canonical",
    defaultValue = Some(true),
    doc = "When this option and spark.sql.nativeView are both true, Spark SQL tries to handle " +
        "CREATE VIEW statement using SQL query string generated from view definition logical " +
        "plan.  If the logical plan doesn't have a SQL representation, we fallback to the " +
        "original native view implementation.",
    isPublic = false)

  val COLUMN_NAME_OF_CORRUPT_RECORD = stringConf(
    "spark.sql.columnNameOfCorruptRecord",
    defaultValue = Some("_corrupt_record"),
    doc =
      "The name of internal column for storing raw/un-parsed JSON records that fail to parse.")

  val BROADCAST_TIMEOUT = intConf(
    "spark.sql.broadcastTimeout",
    defaultValue = Some(5 * 60),
    doc = "Timeout in seconds for the broadcast wait time in broadcast joins.")

  // This is only used for the thriftserver
  val THRIFTSERVER_POOL = stringConf(
    "spark.sql.thriftserver.scheduler.pool",
    doc = "Set a Fair Scheduler pool for a JDBC client session.")

  val THRIFTSERVER_UI_STATEMENT_LIMIT = intConf(
    "spark.sql.thriftserver.ui.retainedStatements",
    defaultValue = Some(200),
    doc = "The number of SQL statements kept in the JDBC/ODBC web UI history.")

  val THRIFTSERVER_UI_SESSION_LIMIT = intConf(
    "spark.sql.thriftserver.ui.retainedSessions",
    defaultValue = Some(200),
    doc =
      "The number of SQL client sessions kept in the JDBC/ODBC web UI history.")

  // This is used to set the default data source
  val DEFAULT_DATA_SOURCE_NAME = stringConf(
    "spark.sql.sources.default",
    defaultValue = Some("org.apache.spark.sql.parquet"),
    doc = "The default data source to use in input/output.")

  // This is used to control the when we will split a schema's JSON string to multiple pieces
  // in order to fit the JSON string in metastore's table property (by default, the value has
  // a length restriction of 4000 characters). We will split the JSON string of a schema
  // to its length exceeds the threshold.
  val SCHEMA_STRING_LENGTH_THRESHOLD = intConf(
    "spark.sql.sources.schemaStringLengthThreshold",
    defaultValue = Some(4000),
    doc = "The maximum length allowed in a single cell when " +
        "storing additional schema information in Hive's metastore.",
    isPublic = false)

  val PARTITION_DISCOVERY_ENABLED = booleanConf(
    "spark.sql.sources.partitionDiscovery.enabled",
    defaultValue = Some(true),
    doc = "When true, automatically discover data partitions.")

  val PARTITION_COLUMN_TYPE_INFERENCE = booleanConf(
    "spark.sql.sources.partitionColumnTypeInference.enabled",
    defaultValue = Some(true),
    doc =
      "When true, automatically infer the data types for partitioned columns.")

  val PARTITION_MAX_FILES = intConf(
    "spark.sql.sources.maxConcurrentWrites",
    defaultValue = Some(1),
    doc = "The maximum number of concurrent files to open before falling back on sorting when " +
        "writing out files using dynamic partitioning.")

  val BUCKETING_ENABLED = booleanConf(
    "spark.sql.sources.bucketing.enabled",
    defaultValue = Some(true),
    doc = "When false, we will treat bucketed table as normal table.")

  val ORDER_BY_ORDINAL = booleanConf(
    "spark.sql.orderByOrdinal",
    defaultValue = Some(true),
    doc = "When true, the ordinal numbers are treated as the position in the select list. " +
        "When false, the ordinal numbers in order/sort By clause are ignored.")

  // The output committer class used by HadoopFsRelation. The specified class needs to be a
  // subclass of org.apache.hadoop.mapreduce.OutputCommitter.
  //
  // NOTE:
  //
  //  1. Instead of SQLConf, this option *must be set in Hadoop Configuration*.
  //  2. This option can be overridden by "spark.sql.parquet.output.committer.class".
  val OUTPUT_COMMITTER_CLASS =
    stringConf("spark.sql.sources.outputCommitterClass", isPublic = false)

  val PARALLEL_PARTITION_DISCOVERY_THRESHOLD = intConf(
    key = "spark.sql.sources.parallelPartitionDiscovery.threshold",
    defaultValue = Some(32),
    doc = "The degree of parallelism for schema merging and partition discovery of " +
        "Parquet data sources.")

  // Whether to perform eager analysis when constructing a dataframe.
  // Set to false when debugging requires the ability to look at invalid query plans.
  val DATAFRAME_EAGER_ANALYSIS = booleanConf(
    "spark.sql.eagerAnalysis",
    defaultValue = Some(true),
    doc = "When true, eagerly applies query analysis on DataFrame operations.",
    isPublic = false)

  // Whether to automatically resolve ambiguity in join conditions for self-joins.
  // See SPARK-6231.
  val DATAFRAME_SELF_JOIN_AUTO_RESOLVE_AMBIGUITY = booleanConf(
    "spark.sql.selfJoinAutoResolveAmbiguity",
    defaultValue = Some(true),
    isPublic = false)

  // Whether to retain group by columns or not in GroupedData.agg.
  val DATAFRAME_RETAIN_GROUP_COLUMNS = booleanConf(
    "spark.sql.retainGroupColumns",
    defaultValue = Some(true),
    isPublic = false)

  val DATAFRAME_PIVOT_MAX_VALUES = intConf(
    "spark.sql.pivotMaxValues",
    defaultValue = Some(10000),
    doc = "When doing a pivot without specifying values for the pivot column this is the maximum " +
        "number of (distinct) values that will be collected without error."
  )

  val RUN_SQL_ON_FILES = booleanConf(
    "spark.sql.runSQLOnFiles",
    defaultValue = Some(true),
    isPublic = false,
    doc = "When true, we could use `datasource`.`path` as table in SQL query.")

  val PARSER_SUPPORT_QUOTEDID = booleanConf(
    "spark.sql.parser.supportQuotedIdentifiers",
    defaultValue = Some(true),
    isPublic = false,
    doc = "Whether to use quoted identifier.\n  false: default(past) behavior. Implies only" +
        "alphaNumeric and underscore are valid characters in identifiers.\n" +
        "  true: implies column names can contain any character.")

  val PARSER_SUPPORT_SQL11_RESERVED_KEYWORDS = booleanConf(
    "spark.sql.parser.supportSQL11ReservedKeywords",
    defaultValue = Some(false),
    isPublic = false,
    doc =
      "This flag should be set to true to enable support for SQL2011 reserved keywords.")

  val WHOLESTAGE_CODEGEN_ENABLED = booleanConf(
    "spark.sql.codegen.wholeStage",
    defaultValue = Some(true),
    doc = "When true, the whole stage (of multiple operators) will be compiled into single java" +
        " method.",
    isPublic = false)

  val FILES_MAX_PARTITION_BYTES = longConf(
    "spark.sql.files.maxPartitionBytes",
    defaultValue = Some(128 * 1024 * 1024), // parquet.block.size
    doc =
      "The maximum number of bytes to pack into a single partition when reading files.",
    isPublic = true)

  val EXCHANGE_REUSE_ENABLED = booleanConf(
    "spark.sql.exchange.reuse",
    defaultValue = Some(true),
    doc =
      "When true, the planner will try to find out duplicated exchanges and re-use them.",
    isPublic = false)

  object Deprecated {
    val MAPRED_REDUCE_TASKS = "mapred.reduce.tasks"
    val EXTERNAL_SORT = "spark.sql.planner.externalSort"
    val USE_SQL_AGGREGATE2 = "spark.sql.useAggregate2"
    val TUNGSTEN_ENABLED = "spark.sql.tungsten.enabled"
    val CODEGEN_ENABLED = "spark.sql.codegen"
    val UNSAFE_ENABLED = "spark.sql.unsafe.enabled"
    val SORTMERGE_JOIN = "spark.sql.planner.sortMergeJoin"
    val PARQUET_UNSAFE_ROW_RECORD_READER_ENABLED =
      "spark.sql.parquet.enableUnsafeRowRecordReader"
  }
}

/**
  * A class that enables the setting and getting of mutable config parameters/hints.
  *
  * In the presence of a SQLContext, these can be set and queried by passing SET commands
  * into Spark SQL's query functions (i.e. sql()). Otherwise, users of this class can
  * modify the hints by programmatically calling the setters and getters of this class.
  *
  * SQLConf is thread-safe (internally synchronized, so safe to be used in multiple threads).
  */
class SQLConf
    extends Serializable
    with CatalystConf
    with ParserConf
    with Logging {
  import SQLConf._

  /** Only low degree of contention is expected for conf, thus NOT using ConcurrentHashMap. */
  @transient protected[spark] val settings = java.util.Collections
    .synchronizedMap(new java.util.HashMap[String, String]())

  /** ************************ Spark SQL Params/Hints ******************* */
  def filesMaxPartitionBytes: Long = getConf(FILES_MAX_PARTITION_BYTES)

  def useCompression: Boolean = getConf(COMPRESS_CACHED)

  def parquetCompressionCodec: String = getConf(PARQUET_COMPRESSION)

  def parquetCacheMetadata: Boolean = getConf(PARQUET_CACHE_METADATA)

  def columnBatchSize: Int = getConf(COLUMN_BATCH_SIZE)

  def numShufflePartitions: Int = getConf(SHUFFLE_PARTITIONS)

  def targetPostShuffleInputSize: Long =
    getConf(SHUFFLE_TARGET_POSTSHUFFLE_INPUT_SIZE)

  def adaptiveExecutionEnabled: Boolean = getConf(ADAPTIVE_EXECUTION_ENABLED)

  def minNumPostShufflePartitions: Int =
    getConf(SHUFFLE_MIN_NUM_POSTSHUFFLE_PARTITIONS)

  def parquetFilterPushDown: Boolean = getConf(PARQUET_FILTER_PUSHDOWN_ENABLED)

  def orcFilterPushDown: Boolean = getConf(ORC_FILTER_PUSHDOWN_ENABLED)

  def verifyPartitionPath: Boolean = getConf(HIVE_VERIFY_PARTITION_PATH)

  def metastorePartitionPruning: Boolean =
    getConf(HIVE_METASTORE_PARTITION_PRUNING)

  def nativeView: Boolean = getConf(NATIVE_VIEW)

  def wholeStageEnabled: Boolean = getConf(WHOLESTAGE_CODEGEN_ENABLED)

  def exchangeReuseEnabled: Boolean = getConf(EXCHANGE_REUSE_ENABLED)

  def canonicalView: Boolean = getConf(CANONICAL_NATIVE_VIEW)

  def caseSensitiveAnalysis: Boolean = getConf(SQLConf.CASE_SENSITIVE)

  def subexpressionEliminationEnabled: Boolean =
    getConf(SUBEXPRESSION_ELIMINATION_ENABLED)

  def autoBroadcastJoinThreshold: Int = getConf(AUTO_BROADCASTJOIN_THRESHOLD)

  def preferSortMergeJoin: Boolean = getConf(PREFER_SORTMERGEJOIN)

  def defaultSizeInBytes: Long =
    getConf(DEFAULT_SIZE_IN_BYTES, autoBroadcastJoinThreshold + 1L)

  def isParquetBinaryAsString: Boolean = getConf(PARQUET_BINARY_AS_STRING)

  def isParquetINT96AsTimestamp: Boolean = getConf(PARQUET_INT96_AS_TIMESTAMP)

  def writeLegacyParquetFormat: Boolean = getConf(PARQUET_WRITE_LEGACY_FORMAT)

  def inMemoryPartitionPruning: Boolean = getConf(IN_MEMORY_PARTITION_PRUNING)

  def columnNameOfCorruptRecord: String =
    getConf(COLUMN_NAME_OF_CORRUPT_RECORD)

  def broadcastTimeout: Int = getConf(BROADCAST_TIMEOUT)

  def defaultDataSourceName: String = getConf(DEFAULT_DATA_SOURCE_NAME)

  def partitionDiscoveryEnabled(): Boolean =
    getConf(SQLConf.PARTITION_DISCOVERY_ENABLED)

  def partitionColumnTypeInferenceEnabled(): Boolean =
    getConf(SQLConf.PARTITION_COLUMN_TYPE_INFERENCE)

  def parallelPartitionDiscoveryThreshold: Int =
    getConf(SQLConf.PARALLEL_PARTITION_DISCOVERY_THRESHOLD)

  def bucketingEnabled: Boolean = getConf(SQLConf.BUCKETING_ENABLED)

  // Do not use a value larger than 4000 as the default value of this property.
  // See the comments of SCHEMA_STRING_LENGTH_THRESHOLD above for more information.
  def schemaStringLengthThreshold: Int =
    getConf(SCHEMA_STRING_LENGTH_THRESHOLD)

  def dataFrameEagerAnalysis: Boolean = getConf(DATAFRAME_EAGER_ANALYSIS)

  def dataFrameSelfJoinAutoResolveAmbiguity: Boolean =
    getConf(DATAFRAME_SELF_JOIN_AUTO_RESOLVE_AMBIGUITY)

  def dataFrameRetainGroupColumns: Boolean =
    getConf(DATAFRAME_RETAIN_GROUP_COLUMNS)

  def runSQLOnFile: Boolean = getConf(RUN_SQL_ON_FILES)

  def supportQuotedId: Boolean = getConf(PARSER_SUPPORT_QUOTEDID)

  def supportSQL11ReservedKeywords: Boolean =
    getConf(PARSER_SUPPORT_SQL11_RESERVED_KEYWORDS)

  override def orderByOrdinal: Boolean = getConf(ORDER_BY_ORDINAL)

  /** ********************** SQLConf functionality methods ************ */
  /** Set Spark SQL configuration properties. */
  def setConf(props: Properties): Unit = settings.synchronized {
    props.asScala.foreach { case (k, v) => setConfString(k, v) }
  }

  /** Set the given Spark SQL configuration property using a `string` value. */
  def setConfString(key: String, value: String): Unit = {
    require(key != null, "key cannot be null")
    require(value != null, s"value cannot be null for key: $key")
    val entry = sqlConfEntries.get(key)
    if (entry != null) {
      // Only verify configs in the SQLConf object
      entry.valueConverter(value)
    }
    setConfWithCheck(key, value)
  }

  /** Set the given Spark SQL configuration property. */
  def setConf[T](entry: SQLConfEntry[T], value: T): Unit = {
    require(entry != null, "entry cannot be null")
    require(value != null, s"value cannot be null for key: ${entry.key}")
    require(sqlConfEntries.get(entry.key) == entry,
            s"$entry is not registered")
    setConfWithCheck(entry.key, entry.stringConverter(value))
  }

  /** Return the value of Spark SQL configuration property for the given key. */
  @throws[NoSuchElementException]("if key is not set")
  def getConfString(key: String): String = {
    Option(settings.get(key))
      .orElse {
        // Try to use the default value
        Option(sqlConfEntries.get(key)).map(_.defaultValueString)
      }
      .getOrElse(throw new NoSuchElementException(key))
  }

  /**
    * Return the value of Spark SQL configuration property for the given key. If the key is not set
    * yet, return `defaultValue`. This is useful when `defaultValue` in SQLConfEntry is not the
    * desired one.
    */
  def getConf[T](entry: SQLConfEntry[T], defaultValue: T): T = {
    require(sqlConfEntries.get(entry.key) == entry,
            s"$entry is not registered")
    Option(settings.get(entry.key))
      .map(entry.valueConverter)
      .getOrElse(defaultValue)
  }

  /**
    * Return the value of Spark SQL configuration property for the given key. If the key is not set
    * yet, return `defaultValue` in [[SQLConfEntry]].
    */
  def getConf[T](entry: SQLConfEntry[T]): T = {
    require(sqlConfEntries.get(entry.key) == entry,
            s"$entry is not registered")
    Option(settings.get(entry.key))
      .map(entry.valueConverter)
      .orElse(entry.defaultValue)
      .getOrElse(throw new NoSuchElementException(entry.key))
  }

  /**
    * Return the `string` value of Spark SQL configuration property for the given key. If the key is
    * not set yet, return `defaultValue`.
    */
  def getConfString(key: String, defaultValue: String): String = {
    val entry = sqlConfEntries.get(key)
    if (entry != null && defaultValue != "<undefined>") {
      // Only verify configs in the SQLConf object
      entry.valueConverter(defaultValue)
    }
    Option(settings.get(key)).getOrElse(defaultValue)
  }

  /**
    * Return all the configuration properties that have been set (i.e. not the default).
    * This creates a new copy of the config properties in the form of a Map.
    */
  def getAllConfs: immutable.Map[String, String] =
    settings.synchronized { settings.asScala.toMap }

  /**
    * Return all the configuration definitions that have been defined in [[SQLConf]]. Each
    * definition contains key, defaultValue and doc.
    */
  def getAllDefinedConfs: Seq[(String, String, String)] =
    sqlConfEntries.synchronized {
      sqlConfEntries.values.asScala
        .filter(_.isPublic)
        .map { entry =>
          (entry.key, entry.defaultValueString, entry.doc)
        }
        .toSeq
    }

  private def setConfWithCheck(key: String, value: String): Unit = {
    if (key.startsWith("spark.") && !key.startsWith("spark.sql.")) {
      logWarning(
        s"Attempt to set non-Spark SQL config in SQLConf: key = $key, value = $value")
    }
    settings.put(key, value)
  }

  def unsetConf(key: String): Unit = {
    settings.remove(key)
  }

  def unsetConf(entry: SQLConfEntry[_]): Unit = {
    settings.remove(entry.key)
  }

  def clear(): Unit = {
    settings.clear()
  }
}
