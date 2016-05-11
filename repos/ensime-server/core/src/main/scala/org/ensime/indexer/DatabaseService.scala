// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.io.File
import java.sql.Timestamp

import akka.event.slf4j.SLF4JLogging
import com.zaxxer.hikari.HikariDataSource
import org.apache.commons.vfs2.FileObject
import org.ensime.indexer.DatabaseService._

import org.ensime.api._
import org.ensime.vfs._

import scala.concurrent._
import scala.concurrent.duration._

import slick.driver.H2Driver.api._

class DatabaseService(dir: File) extends SLF4JLogging {
  lazy val (datasource, db) = {
    // MVCC plus connection pooling speeds up the tests ~10%
    val backend = sys.env.get("ENSIME_EXPERIMENTAL_H2").getOrElse("jdbc:h2:file:")
    val url = backend + dir.getAbsolutePath + "/db;MVCC=TRUE"
    val driver = "org.h2.Driver"

    // https://github.com/brettwooldridge/HikariCP
    val ds = new HikariDataSource()
    ds.setDriverClassName(driver)
    ds.setJdbcUrl(url)
    val threads = ds.getMaximumPoolSize()
    val executor = AsyncExecutor("Slick", numThreads = threads, queueSize = -1)
    (ds, Database.forDataSource(ds, executor = executor))
  }

  def shutdown()(implicit ec: ExecutionContext): Future[Unit] = for {
    // call directly - using slick withSession barfs as it runs a how many rows were updated
    // after shutdown is executed.
    _ <- db.run(sqlu"shutdown")
    _ <- db.shutdown
    _ = datasource.close()
  } yield ()

  if (!dir.exists) {
    log.info("creating the search database...")
    dir.mkdirs()
    Await.result(
      db.run(
        (fileChecks.schema ++ fqnSymbols.schema).create
      ),
      Duration.Inf
    )
    log.info("... created the search database")
  }

  // TODO hierarchy
  // TODO reverse lookup table

  // file with last modified time
  def knownFiles(): Future[Seq[FileCheck]] = db.run(fileChecks.result)

  def removeFiles(files: List[FileObject])(implicit ec: ExecutionContext): Future[Int] =
    db.run {
      val restrict = files.map(_.getName.getURI)
      // Deletion from fqnSymbols relies on fk cascade delete action
      fileChecks.filter(_.filename inSetBind restrict).delete
    }

  private val timestampsQuery = Compiled {
    filename: Rep[String] => fileChecks.filter(_.filename === filename).take(1)
  }

  def outOfDate(f: FileObject)(implicit vfs: EnsimeVFS, ec: ExecutionContext): Future[Boolean] = {
    val uri = f.getName.getURI
    val modified = f.getContent.getLastModifiedTime

    db.run(
      for {
        check <- timestampsQuery(uri).result.headOption
      } yield check.map(_.changed).getOrElse(true)
    )
  }

  def persist(check: FileCheck, symbols: Seq[FqnSymbol])(implicit ec: ExecutionContext): Future[Option[Int]] =
    db.run(
      (fileChecksCompiled += check) andThen (fqnSymbolsCompiled ++= symbols)
    )

  private val findCompiled = Compiled {
    fqn: Rep[String] => fqnSymbols.filter(_.fqn === fqn).take(1)
  }

  def find(fqn: String): Future[Option[FqnSymbol]] = db.run(
    findCompiled(fqn).result.headOption
  )

  import org.ensime.indexer.IndexService._
  def find(fqns: List[FqnIndex])(implicit ec: ExecutionContext): Future[List[FqnSymbol]] = {
    val restrict = fqns.map(_.fqn)
    db.run(
      fqnSymbols.filter(_.fqn inSet restrict).result
    ).map { results =>
      val grouped = results.groupBy(_.fqn)
      restrict.flatMap(grouped.get(_).map(_.head))
    }
  }
}

object DatabaseService {
  case class FileCheck(id: Option[Int], filename: String, timestamp: Timestamp) {
    def file(implicit vfs: EnsimeVFS) = vfs.vfile(filename)
    def lastModified = timestamp.getTime
    def changed(implicit vfs: EnsimeVFS) = file.getContent.getLastModifiedTime != lastModified
  }
  object FileCheck extends ((Option[Int], String, Timestamp) => FileCheck) {
    def apply(f: FileObject): FileCheck = {
      val name = f.getName.getURI
      val ts = new Timestamp(f.getContent.getLastModifiedTime)
      FileCheck(None, name, ts)
    }
  }
  private class FileChecks(tag: Tag) extends Table[FileCheck](tag, "FILECHECKS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def filename = column[String]("filename")
    def timestamp = column[Timestamp]("timestamp")
    def * = (id.?, filename, timestamp) <> (FileCheck.tupled, FileCheck.unapply)
    def idx = index("idx_filename", filename, unique = true)
  }
  private val fileChecks = TableQuery[FileChecks]
  private val fileChecksCompiled = Compiled(TableQuery[FileChecks])

  case class FqnSymbol(
      id: Option[Int],
      file: String, // the underlying file
      path: String, // the VFS handle (e.g. classes in jars)
      fqn: String,
      descriptor: Option[String], // for methods
      internal: Option[String], // for fields
      source: Option[String], // VFS
      line: Option[Int],
      offset: Option[Int] = None // future features:
  //    type: ??? --- better than descriptor/internal
  ) {
    // this is just as a helper until we can use more sensible
    // domain objects with slick
    def sourceFileObject(implicit vfs: EnsimeVFS) = source.map(vfs.vfile)

    // legacy: note that we can't distinguish class/trait
    def declAs: DeclaredAs =
      if (descriptor.isDefined) DeclaredAs.Method
      else if (internal.isDefined) DeclaredAs.Field
      else DeclaredAs.Class
  }
  private class FqnSymbols(tag: Tag) extends Table[FqnSymbol](tag, "FQN_SYMBOLS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def file = column[String]("file")
    def path = column[String]("path")
    def fqn = column[String]("fqn")
    def descriptor = column[Option[String]]("descriptor")
    def internal = column[Option[String]]("internal")
    def source = column[Option[String]]("source handle")
    def line = column[Option[Int]]("line in source")
    def offset = column[Option[Int]]("offset in source")
    def * = (id.?, file, path, fqn, descriptor, internal, source, line, offset) <> (FqnSymbol.tupled, FqnSymbol.unapply)
    def fqnIdx = index("idx_fqn", fqn, unique = false) // fqns are unique by type and sig
    def uniq = index("idx_uniq", (fqn, descriptor, internal), unique = true)
    def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)
  }
  private val fqnSymbols = TableQuery[FqnSymbols]
  private val fqnSymbolsCompiled = Compiled { TableQuery[FqnSymbols] }
}
