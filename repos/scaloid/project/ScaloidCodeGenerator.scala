import ScaloidCodeGenerator._

class ScaloidCodeGenerator(cls: AndroidClass,
                           companionTemplate: CompanionTemplate) {
  import StringUtils._

  private val sClassName = "S" + cls.name

  def implicitConversion = {
    val name = cls.name
    s"$deprecated@inline implicit def ${decapitalize(name)}2Rich$name[V <: ${genType(cls.tpe, erased = true)}]" +
    s"(${decapitalize(name)}: V) = new Rich$name[V](${decapitalize(name)})"
  }

  def wholeClassDef =
    s"""$richClassDef
       |
       |${if (!(cls.isAbstract || cls.isFinal)) prefixedClassDef else ""}
     """.stripMargin

  def richClassDef =
    s"""$richClassScalaDoc
       |${deprecated}class Rich${cls.name}[This <: ${genType(
           cls.tpe,
           erased = true)}](val basis: This) extends $helperTraitName[This]
       |
       |$helperTraitScalaDoc
       |${deprecated}trait $helperTraitName[This <: ${genType(
           cls.tpe,
           erased = true)}]$extendClause {
       |
       |  ${if (cls.parentType.isEmpty) "def basis: This" else ""}
       |
       |  ${companionTemplate.safeRender(cls.name + "_traitBody")}
       |
       |  $properties
       |  $listeners
       |  $intentMethods
       |}
     """.stripMargin

  def deprecated = if (cls.isDeprecated) deprecatedDecl else ""
  def helperTraitName: String = helperTraitName(cls.name)
  def helperTraitName(name: String): String =
    "Trait" + StringUtils.simpleName(name)

  def extendClause = {
    val parent = cls.parentType.map(p => helperTraitName(p.name) + "[This]")
    val mixin = companionTemplate.get(cls.name + "_mixin")
    (parent :: mixin :: Nil).flatten match {
      case Nil => ""
      case head :: tail => s" extends $head" + tail.mkString(" ", " with ", "")
    }
  }

  def prefixedClassDef = {
    val name = cls.name
    if (cls.hasBlankConstructor || CustomClassBodies.toMap.isDefinedAt(name) ||
        companionObjectBodies.toMap.isDefinedAt(name))
      s"""$prefixedClassScalaDoc
         |${deprecated}class S$name$customClassGenerics($customClassExplicitArgs)$classImplicitArgs
         |    extends $baseClassInstance with $helperTraitName[S$name$customSimpleClassGenerics] {
         |
         |  def basis = this
         |  $customClassBodies
         |  ${companionTemplate.safeRender(name + "_concreteBody")}
         |}
         |
         |$companionObjectDef
       """.stripMargin
    else ""
  }

  def companionObjectDef = {
    val con =
      if (!cls.hasBlankConstructor) ""
      else new ConstructorGenerator(cls.constructors.head).constructor

    s"""${deprecated}object $sClassName$customCompanionSuperclass {
       |  $con
       |
       |  $customFullConstructors
       |}
     """.stripMargin
  }

  // Constructors

  def customConstTypeParams = predefinedMapping(constTypeParams)

  def customClassGenerics = predefinedMapping(genericArgs)

  def customSimpleClassGenerics = predefinedMapping(simpleGenericArgs)

  def customClassExplicitArgs = predefinedMapping(ClassExplicitArgs)

  def customBaseClassArgs = predefinedMapping(BaseClassArgs)

  def customClassImplicitArgs = predefinedMapping(ClassImplicitArgs)

  def customClassBodies =
    predefinedMapping(CustomClassBodies, separator = "\n\n")

  def customConstImplicitArgs = predefinedMapping(ConstImplicitArgs)

  def customConstImplicitBodies =
    predefinedMapping(ConstImplicitBodies, separator = "\n")

  def customFullConstructors =
    predefinedMapping(companionObjectBodies, separator = "\n")

  def customCompanionSuperclass =
    predefinedMapping(companionObjectExtends, separator = "\n")

  private def predefinedMapping(mappings: PredefinedCodeMappings,
                                separator: String = ", ") =
    mappings.collect {
      case (kind, fn) if cls.isA(kind) => fn(cls)
    }.mkString(separator)

  class ConstructorGenerator(con: ScalaConstructor) {

    def constructor = {
      val dp = if (con.isDeprecated) deprecatedDecl else ""
      val appliedType = typeVar(cls.tpe)
      s"""${dp}def apply$constTypeParams($constExplicitArgs)$constImplicitArgs: $sClassName$appliedType = {
         |  val v = new $sClassName$appliedType
         |  $customConstImplicitBodies
         |  v
         |}
       """.stripMargin
    }

    private def constExplicitArgs = constArgs(con.explicitArgs)

    private def constImplicitArgs =
      concatArgs(con.implicitArgs, customConstImplicitArgs, isImplicit = true)

    private def constTypeParams = {
      val argStrings =
        con.paramedTypes
          .map(paramedType(_, define = true)) :+ customConstTypeParams.trim
      argStrings.filter(_.nonEmpty) match {
        case Nil => ""
        case params => params.mkString("[", ", ", "]")
      }
    }
  }

  def classImplicitArgs =
    concatArgs(cls.constructors.take(1).flatMap(_.implicitArgs),
               customClassImplicitArgs,
               isImplicit = true)

  private def constArgs(args: List[Argument]): String =
    args.map(a => s"${a.name}: ${genType(a.tpe)}").mkString(", ")

  private def concatArgs(args: List[Argument],
                         customArgs: String,
                         isImplicit: Boolean) = {
    List(constArgs(args), customArgs).filter(_.nonEmpty) match {
      case Nil => ""
      case argStrings =>
        s"(${if (isImplicit) "implicit " else ""}${argStrings.mkString(", ")})"
    }
  }

  def baseClassInstance = {
    val args = BaseClassArgs.toMap
      .get(cls.name)
      .fold(cls.constructors.head.args.map(_.name).mkString(", "))(_ (cls))
    s"${cls.tpe.name}${typeVar(cls.tpe)}($args)"
  }

  def constructors =
    cls.constructors
      .map(new ConstructorGenerator(_).constructor)
      .mkString("\n\n")

  // Methods

  def argTypes(types: List[ScalaType]) = {
    val str = types.map(genType).mkString(", ")
    if (types.length > 1) s"($str)"
    else str
  }

  def namedArgs(types: List[ScalaType]) =
    types match {
      case t :: Nil => "p: " + genType(t)
      case ts =>
        ts.zipWithIndex.map {
          case (t, i) => s"p${i + 1}: ${genType(t)}"
        }.mkString(", ")
    }

  def callArgs(types: List[ScalaType]) =
    types match {
      case t :: Nil => "p"
      case ts =>
        ts.zipWithIndex.map {
          case (_, i) => "p" + (i + 1)
        }.mkString(", ")
    }

  // listener

  def callbackBody(method: AndroidCallbackMethod, isUnit: Boolean = false) =
    if (!method.hasBody) ""
    else if (isUnit) "f"
    else s"f(${callArgs(method.argTypes)})"

  def callbackMethod(m: AndroidCallbackMethod, isUnit: Boolean = false) = {
    s"def ${m.name}(${namedArgs(m.argTypes)}): ${genType(m.retType)} = " +
    s"{ ${callbackBody(m, isUnit)} }"
  }

  def commonListener(l: AndroidListener, args: String = "") = {
    val dp = if (l.isDeprecated) deprecatedDecl else ""
    dp + "@inline def " + l.name + (if (l.retType.name == "Unit")
                                      s"[U](f: $args => U): This = {"
                                    else
                                      s"(f: $args => ${genType(l.retType)}): This = {") +
    s"\n  basis.${l.setter}(new ${l.callbackClassName} {"
  }

  def fullListener(l: AndroidListener) =
    s"""${commonListener(l, argTypes(l.argTypes))}
       |    ${l.callbackMethods.map(callbackMethod(_)).mkString("\n")}
       |  })
       |  basis
       |}""".stripMargin

  def unitListener(l: AndroidListener) =
    s"""${commonListener(l)}
       |    ${l.callbackMethods
         .map(callbackMethod(_, isUnit = true))
         .mkString("\n")}
       |  })
       |  basis
       |}""".stripMargin

  def listener(l: AndroidListener) =
    (if (l.argTypes.nonEmpty) fullListener(l) else "") + "\n\n" + unitListener(
        l)

  def listeners =
    cls.listeners.map(listener).mkString("\n\n")

  // Intent

  def intentMethod(l: AndroidIntentMethod) = {
    val dp = if (l.isDeprecated) deprecatedDecl else ""
    val da = if (l.zeroArgs) "" else s"(${namedArgs(l.argTypes)})"
    val ca = if (l.zeroArgs) "" else s", ${callArgs(l.argTypes)}"
    s"$dp@inline def ${l.name}[T: ClassTag]$da(implicit context: Context): " +
    s"${genType(l.retType)} = basis.${l.name}(SIntent[T]$ca)"
  }

  def intentMethods = cls.intentMethods.map(intentMethod).mkString("\n\n")

  // Property

  def noGetter(name: String) =
    s"""@inline def ${safeIdent(name)}(implicit no: NoGetterForThisProperty): Nothing = throw new Error("Android does not support the getter for '${name}'")"""

  def getter(prop: AndroidProperty) =
    prop.getter.fold(if (prop.nameClashes) "" else noGetter(prop.name)) {
      getter =>
        val dp = if (getter.isDeprecated) deprecatedDecl else ""
        methodScalaDoc(getter) +
        s"\n$dp@inline${if (getter.isOverride) " override"
        else ""} def ${safeIdent(prop.name)} = basis.${getter.name}\n"
    }

  def setter(prop: AndroidProperty, method: AndroidMethod) = {
    def _setter(postFix: String, body: String) =
      if (method.isAbstract && method.paramedTypes.nonEmpty) ""
      else {
        val dp = if (method.isDeprecated) deprecatedDecl else ""
        methodScalaDoc(method) +
        s"\n$dp@inline def ${safeIdent(prop.name + postFix)}${paramedTypes(
            method.paramedTypes)}(${namedArgs(method.argTypes)}) = $body\n"
      }

    _setter("  ", s"            ${prop.name}_=(p)") + "\n" + _setter(
        "_=",
        s"{ basis.${method.name}(p); basis }")
  }

  def switch(name: String, setter: Option[AndroidMethod]) =
    setter.fold("") { s =>
      val dp = if (s.isDeprecated) deprecatedDecl else ""
      val spaces = " " * 13
      s"$dp@inline def  enable$name()$spaces= { basis.${s.name}(true ); basis }\n" +
      s"$dp@inline def disable$name()$spaces= { basis.${s.name}(false); basis }\n"
    }

  def setters(prop: AndroidProperty) =
    prop.setters.map(s => setter(prop, s)).mkString("\n") +
      prop.switch.fold("")("\n\n" + switch(_, prop.setters.headOption))

  def property(prop: AndroidProperty) =
    s"""${getter(prop)}
       |
       |${setters(prop)}
       |""".stripMargin

  def properties = cls.properties.map(property).mkString("\n")

  // Service
  def systemServiceHead =
    s"@inline def ${decapitalize(cls.name)}(implicit context: Context) = \n" +
      s"  context.getSystemService(Context.${managerToService(cls.name)}).asInstanceOf[${cls.tpe.name}]"

  // Scaladoc
  def androidDocBase = "https://developer.android.com/reference"

  def androidClassUrl(c: AndroidClass) =
    s"$androidDocBase/${dotToSlash(c.pkg)}/${c.name}.html"

  def androidReference(c: AndroidClass) =
    s"`[[${androidClassUrl(c)} ${genType(c.tpe, true)}]]`"

  def methodSignature(method: AndroidMethod, sep: String) =
    s"${method.name}(${method.argTypes.map(_.javaName).mkString(sep)})"

  def androidMethodReference(c: AndroidClass, method: AndroidMethod) =
    s"`[[${androidClassUrl(c)}#${methodSignature(method, ",%20")} ${methodSignature(method, ", ")}]]`"

  def methodScalaDoc(method: AndroidMethod) =
    s"""/**
       | * Shortcut for ${androidMethodReference(cls, method)}
       | */
       |""".stripMargin.trim

  def richClassScalaDoc =
    s"""/**
       | * Automatically generated enriching class of ${androidReference(cls)}.
       | */
       |""".stripMargin.trim

  def helperTraitScalaDoc =
    s"""/**
       | * Automatically generated helper trait of ${androidReference(cls)}. This contains several property accessors.
       | */
       |""".stripMargin.trim

  def prefixedClassScalaDoc =
    s"""/**
       | * Automatically generated concrete helper class of ${androidReference(
           cls)}.
       | */
       |""".stripMargin.trim

  def typeVar(tpe: ScalaType, erased: Boolean = false) =
    if (tpe.bounds.nonEmpty) paramedType(tpe)
    else
      tpe.params match {
        case Nil => ""
        case params =>
          s"[${if (erased) "_" else params.map(genType).mkString(", ")}]"
      }

  def paramedType(tpe: ScalaType, define: Boolean = false): String =
    tpe.name + (if (define || !tpe.isVar)
                  " <: " + tpe.bounds.map(genType).mkString(" with ")
                else "")

  def paramedTypes(pTypes: List[ScalaType], define: Boolean = false) =
    if (pTypes.isEmpty) ""
    else pTypes.map(paramedType(_, true)).mkString("[", ", ", "]")

  def genType(tpe: ScalaType): String = genType(tpe, erased = false)
  def genType(tpe: ScalaType, erased: Boolean): String =
    (if (tpe.bounds.isEmpty) tpe.name else "") + typeVar(tpe, erased)
}

object ScaloidCodeGenerator {

  trait CompanionTemplate {

    def get(name: String): Option[String]

    def safeRender(name: String): String
  }

  type PredefinedCodeMapping = (String, (AndroidClass => String))
  type PredefinedCodeMappings = Seq[PredefinedCodeMapping]

  val constTypeParams: PredefinedCodeMappings = List(
      "View" -> { cls =>
        val sClassName = "S" + cls.name
        s"LP <: ViewGroupLayoutParams[_, $sClassName]"
      }
  )

  val genericArgs: PredefinedCodeMappings = List(
      "ArrayAdapter" -> { _ =>
        "[V <: android.view.View, T <: AnyRef]"
      }
  )

  val simpleGenericArgs: PredefinedCodeMappings = List(
      "ArrayAdapter" -> { _ =>
        "[V, T]"
      }
  )

  val ClassExplicitArgs: PredefinedCodeMappings = List(
      "ArrayAdapter" -> { _ =>
        "items: java.util.List[T], textViewResourceId: Int = android.R.layout.simple_spinner_item"
      }
  )

  val BaseClassArgs: PredefinedCodeMappings = List(
      "ArrayAdapter" -> { _ =>
        "context, textViewResourceId, items"
      }
  )

  val ClassImplicitArgs: PredefinedCodeMappings = List(
      "View" -> { _ =>
        "parentVGroup: TraitViewGroup[_] = null"
      }
  )

  val CustomClassBodies: PredefinedCodeMappings = List(
      "View" -> { _ =>
        "override val parentViewGroup = parentVGroup"
      },
      "TextView" -> { _ =>
        s"""|def this(text: CharSequence)(implicit context: Context) = {
          |  this()
          |  this.text = text
          |}
          |
          |def this(text: CharSequence, ignore: Nothing)(implicit context: Context) = this() // Just for implicit conversion of ViewOnClickListener
          |
          |def this(text: CharSequence, onClickListener: ViewOnClickListener, interval: Int)(implicit context: Context) = {
          |  this()
          |  this.text = text
          |  this.setOnClickListener(onClickListener.onClickListener)
          |  if(interval >= 0) onPressAndHold(interval, onClickListener.func(this))
          |}
          |
          |def this(text: CharSequence, onClickListener: ViewOnClickListener)(implicit context: Context) = this(text, onClickListener, -1)
          |
      """.stripMargin
      },
      "ImageView" -> { _ =>
        s"""|def this(imageResource: android.graphics.drawable.Drawable)(implicit context: Context) = {
          |  this()
          |  this.imageDrawable = imageResource
          |}
          |
          |def this(imageResource: android.graphics.drawable.Drawable, ignore: Nothing)(implicit context: Context) = this() // Just for implicit conversion of ViewOnClickListener
          |
          |def this(imageResource: android.graphics.drawable.Drawable, onClickListener: ViewOnClickListener, interval: Int)(implicit context: Context) = {
          |  this()
          |  this.imageDrawable = imageResource
          |  this.setOnClickListener(onClickListener.onClickListener)
          |  if(interval >= 0) onPressAndHold(interval, onClickListener.func(this))
          |}
          |
          |def this(imageResource: android.graphics.drawable.Drawable, onClickListener: ViewOnClickListener)(implicit context: Context) = this(imageResource, onClickListener, -1)
          |
          |""".stripMargin
      }
  )

  val ConstImplicitArgs: PredefinedCodeMappings = List(
      "View" -> { cls =>
        s"defaultLayoutParam: S${cls.name} => LP"
      }
  )

  val ConstImplicitBodies: PredefinedCodeMappings = List(
      "View" -> { _ =>
        "v.<<.parent.+=(v)"
      }
  )

  val companionObjectExtends: PredefinedCodeMappings = List(
      "TextView" -> { cls =>
        val sClassName = "S" + cls.name
        s" extends TextViewCompanion[$sClassName]"
      },
      "ImageView" -> { cls =>
        val sClassName = "S" + cls.name
        s" extends ImageViewCompanion[$sClassName]"
      }
  )

  val companionObjectBodies: PredefinedCodeMappings = List(
      "TextView" -> { cls =>
        val sClassName = "S" + cls.name
        s"def create[LP <: ViewGroupLayoutParams[_, $sClassName]]()(implicit context: Context, defaultLayoutParam: $sClassName => LP) = new $sClassName()"
      },
      "ImageView" -> { cls =>
        val sClassName = "S" + cls.name
        s"def create[LP <: ViewGroupLayoutParams[_, $sClassName]]()(implicit context: Context, defaultLayoutParam: $sClassName => LP) = new $sClassName()"
      },
      "Paint" -> { cls =>
        val sClassName = "S" + cls.name
        s"""|def apply(color: Int): $sClassName = {
          |  val v = new $sClassName
          |  v.color = color
          |  v
          |}
          |""".stripMargin
      },
      "ArrayAdapter" -> { cls =>
        val sClassName = "S" + cls.name
        s"""|def apply[T <: AnyRef : Manifest](items: T*)(implicit context: Context): $sClassName[TextView, T] = new $sClassName[TextView, T](java.util.Arrays.asList[T](items:_*))
          |
          |def apply[T <: AnyRef : Manifest](textViewResourceId: Int, items: T*)(implicit context: Context): $sClassName[TextView, T] = new $sClassName[TextView, T](java.util.Arrays.asList(items:_*), textViewResourceId)
          |
          |def apply[T <: AnyRef](items: Array[T])(implicit context: Context): $sClassName[TextView, T] = new $sClassName[TextView, T](java.util.Arrays.asList(items:_*))
          |
          |def apply[T <: AnyRef](items: java.util.List[T])(implicit context: Context): $sClassName[TextView, T] = new $sClassName[TextView, T](items)
          |
          |def apply[T <: AnyRef](textViewResourceId: Int, items: Array[T])(implicit context: Context): $sClassName[TextView, T] = new $sClassName[TextView, T](java.util.Arrays.asList(items:_*), textViewResourceId)
          |""".stripMargin
      }
  )

  def license =
    """
      |/*
      | *
      | *
      | *
      | *
      | * Scaloid: Simpler Android
      | *
      | * http://scaloid.org
      | *
      | *
      | *
      | *
      | *
      | *
      | * Copyright 2013 Sung-Ho Lee and Scaloid contributors
      | *
      | * Sung-Ho Lee and Scaloid contributors licenses this file to you under the Apache License,
      | * version 2.0 (the "License"); you may not use this file except in compliance
      | * with the License. You may obtain a copy of the License at:
      | *
      | *   http://www.apache.org/licenses/LICENSE-2.0
      | *
      | * Unless required by applicable law or agreed to in writing, software
      | * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
      | * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
      | * License for the specific language governing permissions and limitations
      | * under the License.
      | */
      |
      |/*
      | * This file is automatically generated. Any changes on this file will be OVERWRITTEN!
      | * To learn how to contribute, please refer to:
      | * https://github.com/pocorall/scaloid/wiki/Inside-Scaloid
      | */
      |""".stripMargin
}
