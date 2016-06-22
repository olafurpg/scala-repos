/*     ___ ____ ___   __   ___   ___
 **    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
 **  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2010, LAMP/EPFL
 ** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
 **
 */

package scala.tools.scalap

import java.io.{ByteArrayOutputStream, OutputStreamWriter, PrintStream}

import scala.reflect.internal.pickling.ByteCodecs
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{JavaClassPath, ClassPath}
import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath.{JavaContext, DefaultJavaContext}
import scala.tools.scalap.scalax.rules.scalasig.ClassFileParser.{Annotation, ConstValueIndex}
import scala.tools.scalap.scalax.rules.scalasig._
import scala.tools.util.PathResolver

/**The main object used to execute scalap on the command-line.
  *
  * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
  */
object Main {
  val SCALA_SIG = "ScalaSig"
  val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
  val BYTES_VALUE = "bytes"

  val versionMsg =
    "Scala classfile decoder " + Properties.versionString + " -- " +
      Properties.copyrightString + "\n"

  /**Verbose program run?
    */
  var verbose = false
  var printPrivates = false

  /**Prints usage information for scalap.
    */
  def usage {
    Console.println("usage: scalap {<option>} <name>")
    Console.println(
        "where <name> is fully-qualified class name or <package_name>.package for package objects")
    Console.println("and <option> is")
    Console.println("  -private           print private definitions")
    Console.println("  -verbose           print out additional information")
    Console.println(
        "  -version           print out the version number of scalap")
    Console.println("  -help              display this usage message")
    Console.println(
        "  -classpath <path>  specify where to find user class files")
    Console.println(
        "  -cp <path>         specify where to find user class files")
  }

  def isScalaFile(bytes: Array[Byte]): Boolean = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile.attribute("ScalaSig").isDefined
  }

  /**Processes the given Java class file.
    *
    * @param clazz the class file to be processed.
    */
  def processJavaClassFile(clazz: Classfile) {
    // construct a new output stream writer
    val out = new OutputStreamWriter(Console.out)
    val writer = new JavaWriter(clazz, out)
    // print the class
    writer.printClass
    out.flush()
  }

  def isPackageObjectFile(s: String) =
    s != null && (s.endsWith(".package") || s == "package")

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    val baos = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms = scalaSig.topLevelClasses ::: scalaSig.topLevelObjects
    syms.head.parent match {
      //Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          stream.print("package ")
          stream.print(path)
          stream.print("\n")
        } else {
          val i = path.lastIndexOf(".")
          if (i > 0) {
            stream.print("package ")
            stream.print(path.substring(0, i))
            stream.print("\n")
          }
        }
      }
      case _ =>
    }
    // Print classes
    val printer = new ScalaSigPrinter(stream, printPrivates)
    for (c <- syms) {
      printer.printSymbol(c)
    }
    baos.toString
  }

  def decompileScala(bytes: Array[Byte], isPackageObject: Boolean): String = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile
      .attribute(SCALA_SIG)
      .map(_.byteCode)
      .map(ScalaSigAttributeParsers.parse) match {
      // No entries in ScalaSig attribute implies that the signature is stored in the annotation
      case Some(ScalaSig(_, _, entries)) if entries.length == 0 =>
        unpickleFromAnnotation(classFile, isPackageObject)
      case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
      case None => ""
    }
  }

  def unpickleFromAnnotation(classFile: ClassFile,
                             isPackageObject: Boolean): String = {
    import classFile._
    classFile.annotation(SCALA_SIG_ANNOTATION) match {
      case None => ""
      case Some(Annotation(_, elements)) =>
        val bytesElem = elements
          .find(elem => constant(elem.elementNameIndex) == BYTES_VALUE)
          .get
        val bytes = ((bytesElem.elementValue match {
          case ConstValueIndex(index) => constantWrapped(index)
        }).asInstanceOf[StringBytesPair].bytes)
        val length = ByteCodecs.decode(bytes)
        val scalaSig =
          ScalaSigAttributeParsers.parse(ByteCode(bytes.take(length)))
        parseScalaSignature(scalaSig, isPackageObject)
    }
  }

  /**Executes scalap with the given arguments and classpath for the
    *  class denoted by <code>classname</code>.
    *
    * @param args...
    * @param path...
    * @param classname...
    */
  def process(args: Arguments, path: ClassPath[AbstractFile])(
      classname: String) {
    // find the classfile
    val encName = Names.encode(
        if (classname == "scala.AnyRef") "java.lang.Object"
        else classname)
    val cls = path.findClass(encName)
    if (cls.isDefined && cls.get.binary.isDefined) {
      val cfile = cls.get.binary.get
      if (verbose) {
        Console.println(
            Console.BOLD + "FILENAME" + Console.RESET + " = " + cfile.path)
      }
      val bytes = cfile.toByteArray
      if (isScalaFile(bytes)) {
        Console.println(decompileScala(bytes, isPackageObjectFile(encName)))
      } else {
        // construct a reader for the classfile content
        val reader = new ByteArrayReader(cfile.toByteArray)
        // parse the classfile
        val clazz = new Classfile(reader)
        processJavaClassFile(clazz)
      }
      // if the class corresponds to the artificial class scala.Any.
      // (see member list in class scala.tool.nsc.symtab.Definitions)
    } else if (classname == "scala.Any") {
      Console.println("package scala")
      Console.println("class Any {")
      Console.println("  final def ==(scala.Any): scala.Boolean")
      Console.println("  final def !=(scala.Any): Boolean")
      Console.println("  def equals(scala.Any): scala.Boolean")
      Console.println("  def hashCode(): scala.Int")
      Console.println("  def toString(): java.lang.String")
      Console.println("  final def isInstanceOf[a]: scala.Boolean")
      Console.println("  final def asInstanceOf[a]: a")
      Console.println("}")
      // if the class corresponds to the artificial class scala.AnyRef.
    } else if (classname == "scala.AnyRef") {
      Console.println("package scala")
      Console.println("class AnyRef extends Any {")
      Console.println("  def equals(scala.Any): scala.Boolean")
      Console.println("  def hashCode(): scala.Int")
      Console.println("  def toString(): java.lang.String")
      Console.println("}")
      // if the class corresponds to the artificial class scala.AnyVal.
    } else if (classname == "scala.AnyVal") {
      Console.println("package scala")
      Console.println("sealed class AnyVal extends Any")
      // if the class corresponds to the artificial class scala.Boolean.
    } else if (classname == "scala.Boolean") {
      Console.println("package scala")
      Console.println("sealed abstract class Boolean extends AnyVal {")
      Console.println(
          "  def &&(p: => scala.Boolean): scala.Boolean  // boolean and")
      Console.println(
          "  def ||(p: => scala.Boolean): scala.Boolean  // boolean or")
      Console.println(
          "  def & (x: scala.Boolean): scala.Boolean     // boolean strict and")
      Console.println(
          "  def | (x: scala.Boolean): scala.Boolean     // boolean stric or")
      Console.println(
          "  def ==(x: scala.Boolean): scala.Boolean     // boolean equality")
      Console.println(
          "  def !=(x: scala.Boolean): scala.Boolean     // boolean inequality")
      Console.println(
          "  def !: scala.Boolean                        // boolean negation")
      Console.println("}")
      // if the class corresponds to the artificial class scala.Int.
    } else if (classname == "scala.Int") {
      Console.println("package scala")
      Console.println("sealed abstract class Int extends AnyVal {")
      Console.println("  def ==(that: scala.Double): scala.Boolean")
      Console.println("  def ==(that: scala.Float): scala.Boolean")
      Console.println("  def ==(that: scala.Long): scala.Boolean")
      Console.println("  def ==(that: scala.Int): scala.Boolean")
      Console.println("  def ==(that: scala.Short): scala.Boolean")
      Console.println("  def ==(that: scala.Byte): scala.Boolean")
      Console.println("  def ==(that: scala.Char): scala.Boolean")
      Console.println("  /* analogous for !=, <, >, <=, >= */")
      Console.println
      Console.println(
          "  def + (that: scala.Double): scala.Double // double addition")
      Console.println(
          "  def + (that: scala.Float): scala.Float   // float addition")
      Console.println(
          "  def + (that: scala.Long): scala.Long     // long addition")
      Console.println(
          "  def + (that: scala.Int): scala.Int       // int addition")
      Console.println(
          "  def + (that: scala.Short): scala.Int     // int addition")
      Console.println(
          "  def + (that: scala.Byte): scala.Int      // int addition")
      Console.println(
          "  def + (that: scala.Char): scala.Int      // int addition")
      Console.println("  /* analogous for -, *, /, % */")
      Console.println
      Console.println(
          "  def & (that: scala.Long): scala.Long     // long bitwise and")
      Console.println(
          "  def & (that: scala.Int): scala.Int       // int bitwise and")
      Console.println(
          "  def & (that: scala.Short): scala.Int     // int bitwise and")
      Console.println(
          "  def & (that: scala.Byte): scala.Int      // int bitwise and")
      Console.println(
          "  def & (that: scala.Char): scala.Int      // int bitwise and")
      Console.println("  /* analogous for |, ^ */")
      Console.println
      Console.println(
          "  def <<(cnt: scala.Int): scala.Int        // int left shift")
      Console.println(
          "  def <<(cnt: scala.Long): scala.Int       // long left shift")
      Console.println("  /* analogous for >>, >>> */")
      Console.println
      Console.println(
          "  def + : scala.Int                        // int identity")
      Console.println(
          "  def - : scala.Int                        // int negation")
      Console.println(
          "  def ~ : scala.Int                        // int bitwise negation")
      Console.println
      Console.println(
          "  def toByte: scala.Byte                   // convert to Byte")
      Console.println(
          "  def toShort: scala.Short                 // convert to Short")
      Console.println(
          "  def toChar: scala.Char                   // convert to Char")
      Console.println(
          "  def toInt: scala.Int                     // convert to Int")
      Console.println(
          "  def toLong: scala.Long                   // convert to Long")
      Console.println(
          "  def toFloat: scala.Float                 // convert to Float")
      Console.println(
          "  def toDouble: scala.Double               // convert to Double")
      Console.println("}")
      // if the class corresponds to the artificial class scala.Nothing.
    } else if (classname == "scala.Nothing") {
      Console.println("package scala")
      Console.println("sealed abstract class Nothing")
      // if the class corresponds to the artificial class scala.Null.
    } else if (classname == "scala.Null") {
      Console.println("package scala")
      Console.println("sealed abstract class Null")
    } else Console.println("class/object " + classname + " not found.")
  }

  def fromPathString(
      path: String,
      context: JavaContext = DefaultJavaContext): JavaClassPath = {
    val s = new Settings()
    s.classpath.value = path
    new PathResolver(s, context).result
  }

  /**The main method of this object.
    */
  def main(args: Array[String]) {
    // print usage information if there is no command-line argument
    if (args.length == 0) usage
    // otherwise parse the arguments...
    else {
      val arguments = Arguments
        .Parser('-')
        .withOption("-private")
        .withOption("-verbose")
        .withOption("-version")
        .withOption("-help")
        .withOptionalArg("-classpath")
        .withOptionalArg("-cp")
        .parse(args)
      if (arguments contains "-version") Console.println(versionMsg)
      if (arguments contains "-help") usage
      verbose = arguments contains "-verbose"
      printPrivates = arguments contains "-private"
      // construct a custom class path
      def cparg =
        List("-classpath", "-cp") map (arguments getArgument _) reduceLeft
          (_ orElse _)
      val path = cparg map (fromPathString(_)) getOrElse EmptyClasspath
      // print the classpath if output is verbose
      if (verbose) {
        Console.println(
            Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)
      }
      // process all given classes
      arguments.getOthers.foreach(process(arguments, path))
    }
  }

  object EmptyClasspath extends ClassPath[AbstractFile] {

    /**
      * The short name of the package (without prefix)
      */
    def name: String = ""
    def asURLs = Nil
    override def asClasspathString = ""
    override def asClassPathString: String = ""
    val context = DefaultJavaContext
    val classes: IndexedSeq[ClassPath[AbstractFile]#ClassRep] =
      IndexedSeq.empty
    val packages: IndexedSeq[ClassPath[AbstractFile]] = IndexedSeq.empty
    val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq.empty
  }
}
