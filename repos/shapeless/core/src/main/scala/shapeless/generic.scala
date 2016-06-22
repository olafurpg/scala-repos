/*
 * Copyright (c) 2012-16 Lars Hupel, Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import scala.language.experimental.macros

import scala.annotation.{StaticAnnotation, tailrec}
import scala.reflect.macros.{blackbox, whitebox}

import ops.{hlist, coproduct}

/** Represents the ability to convert from a concrete type (e.g. a case class)
  * to a generic ([[HList]] / [[Coproduct]]} based) representation of the type.
  *
  * For example:
  * {{{
  * scala> sealed trait Animal

  * defined trait Animal
  * scala> case class Cat(name: String, livesLeft: Int) extends Animal
  * defined class Cat
  *
  * scala> case class Dog(name: String, bonesHidden: Int) extends Animal
  * defined class Dog
  *
  * scala> val genCat = Generic[Cat]
  * genCat: shapeless.Generic[Cat]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val genDog = Generic[Dog]
  * genDog: shapeless.Generic[Dog]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val garfield = Cat("Garfield", 9)
  * garfield: Cat = Cat(Garfield,9)
  *
  * scala> val genGarfield = genCat.to(garfield)
  * genGarfield: genCat.Repr = Garfield :: 9 :: HNil
  *
  * scala> val reconstructed = genCat.from(genGarfield)
  * reconstructed: Cat = Cat(Garfield,9)
  *
  * scala> reconstructed == garfield
  * res0: Boolean = true
  *
  * }}}
  *
  * Note that constituents of Cat and Dog are exactly the same - a String and an Int. So we could do:
  *
  * {{{
  *
  * scala> val odieAsCat = genCat.from(genDog.to(odie))
  * odieAsCat: Cat = Cat(odie,3)
  *
  * }}}
  *
  * This is quite useful in certain cases, such as copying from one object type to another, as in schema evolution.
  *
  * Note that the generic representation depends on the type at which we instantiate Generic. In the
  * example above we instantiated it at Cat and at Dog, and so the generic representation gave the minimal constituents
  * of each of those.
  *
  * However, if we instantiate Generic[Animal] instead the generic representation would encode
  * the Cat-ness or Dog-ness of the instance as well (see [[Coproduct]] for details of the encoding):
  *
  * {{{
  *
  * scala> genDog.to(odie)
  * res9: genDog.Repr = odie :: 3 :: HNil
  *
  * scala> val genAnimal = Generic[Animal]
  * genAnimal: shapeless.Generic[Animal]{ type Repr = Cat :+: Dog :+: CNil } = ...
  *
  * scala> genAnimal.to(odie)
  * res8: genAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog; case _ => null }
  * res9: Dog = Dog(odie,3)
  *
  * }}}
  *
  * Inr and Inl are [[shapeless.Coproduct]] constructors.
  * Shapeless constructs each class representation as a sort of
  * "nested Either" using Coproduct. So in our example, genAnimal would essentially encode garfield as Inl(garfield)
  * and odie as Inr(Inl(odie)). Please see [[shapeless.Coproduct]] for more details.
  * }}}
  *
  * @tparam T  An immutable data type that has a canonical way of constructing and deconstructing
  *            instances (e.g. via apply / unapply). Sealed families of case classes work best.
  */
trait Generic[T] extends Serializable {

  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t: T): Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r: Repr): T
}

/** The companion object for the [[Generic]] trait provides a way of obtaining a Generic[T] instance
  * for some T. In addition, it defines [[Generic.Aux]], which is an important implementation technique
  * that can be generally useful.
  */
object Generic {

  /** Provides a representation of Generic[T], which has a nested Repr type, as a type with two type
    * parameters instead.
    *
    * This is useful for two reasons. First, it's surprisingly easy to wind up with a Generic type that
    * has lost the refinement that carries the crucial Generic.Repr type, a problem which Generic.Aux prevents.
    *
    * More importantly, Aux allows us to write code like this:
    *
    * {{{
    *   def myMethod[T]()(implicit eqGen: Generic.Aux[T,R], repEq: Eq[R]) = ???
    * }}}
    *
    * Here, we specify T, and we find a Generic.Aux[T,R] by implicit search. We then use R in the second argument.
    * Generic.Aux[T, R] is exactly equivalent to Generic[T] { type Repr = R }, but Scala doesn't allow us to write
    * it this way:
    *
    * {{{
    *   def myMethod[T]()(eqGen: Generic[T] { Repr = R }, reqEq: Eq[egGen.Repr) = ???
    * }}}
    *
    * The reason is that we are not allowed to have dependencies between arguments in the same parameter group. So
    * Aux neatly sidesteps this problem.
    *
    * The "Aux pattern" is now in use in several other libraries as well, and is a useful general technique.
    *
    * @tparam T the type for which we want to find a Generic
    * @tparam Repr0 the generic representation type equivalent to T.
    */
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  /** Provides an instance of Generic. Prefer this over finding one with `implicitly`, or else use `the`.
    *
    * Either of these approaches preserves the Repr type refinement, which `implicitly` will lose.
    *
    */
  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  implicit def materialize[T, R]: Aux[T, R] = macro GenericMacros
    .materialize[T, R]
}

/**
  * LabelledGeneric is similar to Generic, but includes information about field
  * names or class names in addition to the raw structure.
  *
  * Continuing the example from [[shapeless.Generic]], we use LabelledGeneric to convert an object to an [[shapeless.HList]]:
  *
  * {{{
  * scala> val lgenDog = LabelledGeneric[Dog]
  * lgenDog: shapeless.LabelledGeneric[Dog]{ type Repr = Record.`'name -> String, 'bonesHidden -> Int`.T } = ...
  *
  * scala> lgenDog.to(odie)
  * res15: lgenDog.Repr = odie :: 3 :: HNil
  * }}}
  *
  * Note that the representation does not include the labels! The labels are actually encoded in the generic type representation
  * using [[shapeless.Witness]] types.
  *
  * As with [[shapeless.Generic]], the representation for Animal captures the subclass embedding rather than the fields in the class,
  * using [[shapeless.Coproduct]]:
  *
  * {{{
  * scala> val lgenAnimal = LabelledGeneric[Animal]
  * lgenAnimal: shapeless.LabelledGeneric[Animal]{ type Repr = Union.`'Cat -> Cat, 'Dog -> Dog`.T } = ...
  *
  * scala> lgenAnimal.to(odie)
  * res16: lgenAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog ; case _ => ???}
  * res19: Dog = Dog(odie,3)
  *
  * }}}
  *
  * @tparam T the type which this instance can convert to and from a labelled generic representation
  */
trait LabelledGeneric[T] extends Serializable {

  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t: T): Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r: Repr): T
}

object LabelledGeneric {

  /** Like [[shapeless.Generic.Aux]], this is an implementation of the Aux pattern, please
    * see comments there.
    * @tparam T the type
    * @tparam Repr0 the labelled generic representation of the type
    */
  type Aux[T, Repr0] = LabelledGeneric[T] { type Repr = Repr0 }

  /** Provides an instance of LabelledGeneric for the given T. As with [[shapeless.Generic]],
    * use this method or {{{the[LabelledGeneric[T]]}}} to obtain an instance for suitable given T. */
  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  /** Handles the Product case (fields in a case class, for example) */
  implicit def materializeProduct[T, K <: HList, V <: HList, R <: HList](
      implicit lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: hlist.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }

  /** Handles the Coproduct case (specifying subclasses derive from a sealed trait) */
  implicit def materializeCoproduct[T,
                                    K <: HList,
                                    V <: Coproduct,
                                    R <: Coproduct](
      implicit lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: coproduct.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }
}

class nonGeneric extends StaticAnnotation

class IsTuple[T] extends Serializable

object IsTuple {
  implicit def apply[T]: IsTuple[T] = macro GenericMacros.mkIsTuple[T]
}

class HasProductGeneric[T] extends Serializable

object HasProductGeneric {
  implicit def apply[T]: HasProductGeneric[T] = macro GenericMacros
    .mkHasProductGeneric[T]
}

class HasCoproductGeneric[T] extends Serializable

object HasCoproductGeneric {
  implicit def apply[T]: HasCoproductGeneric[T] = macro GenericMacros
    .mkHasCoproductGeneric[T]
}

@macrocompat.bundle
trait ReprTypes {
  val c: blackbox.Context
  import c.universe.{Symbol => _, _}

  def hlistTpe = typeOf[HList]
  def hnilTpe = typeOf[HNil]
  def hconsTpe = typeOf[::[_, _]].typeConstructor
  def coproductTpe = typeOf[Coproduct]
  def cnilTpe = typeOf[CNil]
  def cconsTpe = typeOf[:+:[_, _]].typeConstructor

  def atatTpe = typeOf[tag.@@[_, _]].typeConstructor
  def fieldTypeTpe = typeOf[shapeless.labelled.FieldType[_, _]].typeConstructor
  def keyTagTpe = typeOf[shapeless.labelled.KeyTag[_, _]].typeConstructor
  def symbolTpe = typeOf[Symbol]
}

@macrocompat.bundle
trait CaseClassMacros extends ReprTypes {
  val c: whitebox.Context

  import c.universe._
  import internal.constantType
  import Flag._

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def isReprType(tpe: Type): Boolean =
    tpe <:< hlistTpe || tpe <:< coproductTpe

  def isReprType1(tpe: Type): Boolean = {
    val normalized = appliedType(tpe, WildcardType).dealias
    normalized <:< hlistTpe || normalized <:< coproductTpe
  }

  def lowerKind(tpe: Type): Type =
    if (tpe.takesTypeArgs) appliedType(tpe, List(typeOf[Any])).dealias
    else tpe

  def isProductAux(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && (isCaseClassLike(classSym(tpe)) ||
          HasApplyUnapply(tpe) || HasCtorUnapply(tpe))

  def isProduct(tpe: Type): Boolean =
    tpe =:= typeOf[Unit] || (!(tpe =:= typeOf[AnyRef]) && isProductAux(tpe))

  def isProduct1(tpe: Type): Boolean =
    lowerKind(tpe) =:= typeOf[Unit] ||
      (!(lowerKind(tpe) =:= typeOf[AnyRef]) && isProductAux(tpe))

  def isCoproduct(tpe: Type): Boolean = {
    val sym = tpe.typeSymbol
    if (!sym.isClass) false
    else {
      val sym = classSym(tpe)
      (sym.isTrait || sym.isAbstract) && sym.isSealed
    }
  }

  def ownerChain(sym: Symbol): List[Symbol] = {
    @tailrec
    def loop(sym: Symbol, acc: List[Symbol]): List[Symbol] =
      if (sym.owner == NoSymbol) acc
      else loop(sym.owner, sym :: acc)

    loop(sym, Nil)
  }

  def mkDependentRef(prefix: Type, path: List[Name]): (Type, Symbol) = {
    val (_, pre, sym) = path.foldLeft((prefix, NoType, NoSymbol)) {
      case ((pre, _, sym), nme) =>
        val sym0 = pre.member(nme)
        val pre0 = sym0.typeSignature
        (pre0, pre, sym0)
    }
    (pre, sym)
  }

  def isAnonOrRefinement(sym: Symbol): Boolean = {
    val nameStr = sym.name.toString
    nameStr.contains("$anon") || nameStr == "<refinement>"
  }

  def fieldsOf(tpe: Type): List[(TermName, Type)] = {
    val tSym = tpe.typeSymbol
    if (tSym.isClass && isAnonOrRefinement(tSym)) Nil
    else
      tpe.decls.toList collect {
        case sym: TermSymbol if isCaseAccessorLike(sym) =>
          (sym.name.toTermName, sym.typeSignatureIn(tpe).finalResultType)
      }
  }

  def productCtorsOf(tpe: Type): List[Symbol] =
    tpe.decls.toList.filter(_.isConstructor)

  def accessiblePrimaryCtorOf(tpe: Type): Option[Symbol] = {
    for {
      ctor <- tpe.decls.find { sym =>
               sym.isMethod && sym.asMethod.isPrimaryConstructor &&
               isAccessible(tpe, sym)
             } if !ctor.isJava || productCtorsOf(tpe).size == 1
    } yield ctor
  }

  def ctorsOf(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, false)
  def ctorsOf1(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, true)

  def distinctCtorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def distinct[A](list: List[A])(eq: (A, A) => Boolean): List[A] =
      list
        .foldLeft(List.empty[A]) { (acc, x) =>
          if (!acc.exists(eq(x, _))) x :: acc
          else acc
        }
        .reverse
    distinct(ctorsOfAux(tpe, hk))(_ =:= _)
  }

  def ctorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def collectCtors(classSym: ClassSymbol): List[ClassSymbol] = {
      classSym.knownDirectSubclasses.toList flatMap { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
        if (isCaseClassLike(child) || isCaseObjectLike(child)) List(child)
        else if (child.isSealed) collectCtors(child)
        else abort(s"$child is not case class like or a sealed trait")
      }
    }

    if (isProduct(tpe)) List(tpe)
    else if (isCoproduct(tpe)) {
      val basePre = prefix(tpe)
      val baseSym = classSym(tpe)
      val baseTpe =
        if (!hk) tpe
        else {
          val tc = tpe.typeConstructor
          val paramSym = tc.typeParams.head
          val paramTpe = paramSym.asType.toType
          appliedType(tc, paramTpe)
        }
      val baseArgs = baseTpe.dealias.typeArgs

      val ctorSyms = collectCtors(baseSym).sortBy(_.fullName)
      val ctors =
        ctorSyms flatMap { sym =>
          def substituteArgs: List[Type] = {
            val subst = c.internal.thisType(sym).baseType(baseSym).typeArgs
            sym.typeParams.map { param =>
              val paramTpe = param.asType.toType
              baseArgs(subst.indexWhere(_ =:= paramTpe))
            }
          }

          val suffix = ownerChain(sym).dropWhile(_ != basePre.typeSymbol)
          val ctor = if (suffix.isEmpty) {
            if (sym.isModuleClass) {
              val moduleSym = sym.asClass.module
              val modulePre = prefix(moduleSym.typeSignature)
              c.internal.singleType(modulePre, moduleSym)
            } else appliedType(sym.toTypeIn(basePre), substituteArgs)
          } else {
            if (sym.isModuleClass) {
              val path = suffix.tail.map(_.name.toTermName)
              val (modulePre, moduleSym) = mkDependentRef(basePre, path)
              c.internal.singleType(modulePre, moduleSym)
            } else if (isAnonOrRefinement(sym)) {
              val path = suffix.tail.init.map(_.name.toTermName)
              val (valPre, valSym) = mkDependentRef(basePre, path)
              c.internal.singleType(valPre, valSym)
            } else {
              val path =
                suffix.tail.init
                  .map(_.name.toTermName) :+ suffix.last.name.toTypeName
              val (subTpePre, subTpeSym) = mkDependentRef(basePre, path)
              c.internal.typeRef(subTpePre, subTpeSym, substituteArgs)
            }
          }
          if (!isAccessible(ctor))
            abort(s"$tpe has an inaccessible subtype $ctor")
          if (ctor <:< baseTpe) Some(ctor) else None
        }
      if (ctors.isEmpty) abort(s"Sealed trait $tpe has no case class subtypes")
      ctors
    } else
      abort(
          s"$tpe is not a case class, case class-like, a sealed trait or Unit")
  }

  def nameAsString(name: Name): String = name.decodedName.toString.trim

  def nameAsValue(name: Name): Constant = Constant(nameAsString(name))

  def nameOf(tpe: Type) = tpe.typeSymbol.name

  def mkHListValue(elems: List[Tree]): Tree =
    elems.foldRight(q"_root_.shapeless.HNil": Tree) {
      case (elem, acc) => q"_root_.shapeless.::($elem, $acc)"
    }

  def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
    items.foldRight(nil) {
      case (tpe, acc) => appliedType(cons, List(devarargify(tpe), acc))
    }

  def mkLabelTpe(name: Name): Type =
    appliedType(atatTpe,
                List(typeOf[scala.Symbol], constantType(nameAsValue(name))))

  def mkFieldTpe(name: Name, valueTpe: Type): Type = {
    appliedType(fieldTypeTpe, List(mkLabelTpe(name), valueTpe))
  }

  def mkHListTpe(items: List[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  def mkCoproductTpe(items: List[Type]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, items)

  def unpackHListTpe(tpe: Type): List[Type] = {
    @tailrec
    def unfold(u: Type, acc: List[Type]): List[Type] = {
      val HNilTpe = hnilTpe
      val HConsPre = prefix(hconsTpe)
      val HConsSym = hconsTpe.typeSymbol
      if (u <:< HNilTpe) acc
      else
        (u baseType HConsSym) match {
          case TypeRef(pre, _, List(hd, tl)) if pre =:= HConsPre =>
            unfold(tl, hd :: acc)
          case _ => abort(s"$tpe is not an HList type")
        }
    }

    unfold(tpe, List()).reverse
  }

  def unpackFieldType(tpe: Type): (Type, Type) = {
    val KeyTagPre = prefix(keyTagTpe)
    val KeyTagSym = keyTagTpe.typeSymbol
    tpe.dealias match {
      case RefinedType(List(v0, TypeRef(pre, KeyTagSym, List(k, v1))), _)
          if pre =:= KeyTagPre && v0 =:= v1 =>
        (k, v0)
      case _ => abort(s"$tpe is not a field type")
    }
  }

  def mkTypTree(tpe: Type): Tree = {
    tpe match {
      case SingleType(pre @ SingleType(_, _), sym) =>
        SingletonTypeTree(mkAttributedRef(pre, sym))

      case TypeRef(pre, _, args) if isVararg(tpe) =>
        val argTrees = args.map(mkTypTree)
        AppliedTypeTree(tq"_root_.scala.collection.Seq", argTrees)

      case t => tq"$t"
    }
  }

  def appliedTypTree1(tpe: Type, param: Type, arg: TypeName): Tree = {
    tpe match {
      case t if t =:= param =>
        Ident(arg)
      case PolyType(params, body) if params.head.asType.toType =:= param =>
        appliedTypTree1(body, param, arg)
      case t @ TypeRef(pre, sym, List()) if t.takesTypeArgs =>
        val argTrees = t.typeParams.map(sym =>
              appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(mkAttributedRef(pre, sym), argTrees)
      case TypeRef(pre, sym, List()) =>
        mkAttributedRef(pre, sym)
      case TypeRef(pre, sym, args) =>
        val argTrees = args.map(appliedTypTree1(_, param, arg))
        AppliedTypeTree(mkAttributedRef(pre, sym), argTrees)
      case t if t.takesTypeArgs =>
        val argTrees = t.typeParams.map(sym =>
              appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(mkAttributedRef(tpe.typeConstructor), argTrees)
      case t =>
        tq"$tpe"
    }
  }

  def mkCompoundTypTree(nil: Type, cons: Type, items: List[Type]): Tree =
    items.foldRight(mkAttributedRef(nil): Tree) {
      case (tpe, acc) =>
        AppliedTypeTree(mkAttributedRef(cons), List(mkTypTree(tpe), acc))
    }

  def mkCompoundTypTree1(nil: Type,
                         cons: Type,
                         items: List[Type],
                         param: Type,
                         arg: TypeName): Tree =
    items.foldRight(mkAttributedRef(nil): Tree) {
      case (tpe, acc) =>
        AppliedTypeTree(mkAttributedRef(cons),
                        List(appliedTypTree1(tpe, param, arg), acc))
    }

  def mkHListTypTree(items: List[Type]): Tree =
    mkCompoundTypTree(hnilTpe, hconsTpe, items)

  def mkHListTypTree1(items: List[Type], param: Type, arg: TypeName): Tree =
    mkCompoundTypTree1(hnilTpe, hconsTpe, items, param, arg)

  def mkCoproductTypTree(items: List[Type]): Tree =
    mkCompoundTypTree(cnilTpe, cconsTpe, items)

  def mkCoproductTypTree1(items: List[Type],
                          param: Type,
                          arg: TypeName): Tree =
    mkCompoundTypTree1(cnilTpe, cconsTpe, items, param, arg)

  def unfoldCompoundTpe(compoundTpe: Type, nil: Type, cons: Type): List[Type] = {
    @tailrec
    def loop(tpe: Type, acc: List[Type]): List[Type] =
      tpe.dealias match {
        case TypeRef(_, consSym, List(hd, tl))
            if consSym.asType.toType.typeConstructor =:= cons =>
          loop(tl, hd :: acc)
        case `nil` => acc
        case other => abort(s"Bad compound type $compoundTpe")
      }
    loop(compoundTpe, Nil).reverse
  }

  def hlistElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, hnilTpe, hconsTpe)

  def coproductElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, cnilTpe, cconsTpe)

  def reprTpe(tpe: Type): Type = {
    if (isProduct(tpe)) mkHListTpe(fieldsOf(tpe).map(_._2))
    else mkCoproductTpe(ctorsOf(tpe))
  }

  def param1(tpe: Type): Type =
    tpe match {
      case t if (tpe.takesTypeArgs) => t.typeParams.head.asType.toType
      case TypeRef(_, _, List(arg)) => arg
      case _ => NoType
    }

  def reprTypTree(tpe: Type): Tree = {
    if (isProduct(tpe)) mkHListTypTree(fieldsOf(tpe).map(_._2))
    else mkCoproductTypTree(ctorsOf(tpe))
  }

  def reprTypTree1(tpe: Type, arg: TypeName): Tree = {
    val param = param1(tpe)
    if (isProduct1(tpe)) mkHListTypTree1(fieldsOf(tpe).map(_._2), param, arg)
    else mkCoproductTypTree1(ctorsOf1(tpe), param, arg)
  }

  def isCaseClassLike(sym: ClassSymbol): Boolean = {
    def checkCtor: Boolean = {
      def unique[T](s: Seq[T]): Option[T] =
        s.headOption.find(_ => s.tail.isEmpty)

      val tpe = sym.typeSignature
      (for {
        ctor <- accessiblePrimaryCtorOf(tpe)
        params <- unique(ctor.asMethod.paramLists)
      } yield params.size == fieldsOf(tpe).size).getOrElse(false)
    }

    sym.isCaseClass ||
    (!sym.isAbstract && !sym.isTrait && !(sym == symbolOf[Object]) &&
        sym.knownDirectSubclasses.isEmpty && checkCtor)
  }

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass

  def isCaseAccessorLike(sym: TermSymbol): Boolean =
    !isNonGeneric(sym) && sym.isPublic &&
      (if (sym.owner.asClass.isCaseClass) sym.isCaseAccessor
       else sym.isAccessor)

  def isSealedHierarchyClassSymbol(symbol: ClassSymbol): Boolean = {
    def helper(classSym: ClassSymbol): Boolean = {
      classSym.knownDirectSubclasses.toList forall { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

        isCaseClassLike(child) || (child.isSealed && helper(child))
      }
    }

    symbol.isSealed && helper(symbol)
  }

  def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass) abort(s"$sym is not a class or trait")

    val classSym = sym.asClass
    classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

    classSym
  }

  // See https://github.com/milessabin/shapeless/issues/212
  def companionRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val cSym =
      patchedCompanionSymbolOf(tpe.typeSymbol).asInstanceOf[global.Symbol]
    if (cSym != NoSymbol)
      global.gen.mkAttributedRef(pre, cSym).asInstanceOf[Tree]
    else
      Ident(tpe.typeSymbol.name.toTermName) // Attempt to refer to local companion
  }

  def isAccessible(pre: Type, sym: Symbol): Boolean = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c
      .asInstanceOf[scala.reflect.macros.runtime.Context]
      .callsiteTyper
      .asInstanceOf[global.analyzer.Typer]
    val typerContext = typer.context
    typerContext.isAccessible(
        sym.asInstanceOf[global.Symbol],
        pre.asInstanceOf[global.Type]
    )
  }
  def isAccessible(tpe: Type): Boolean =
    isAccessible(prefix(tpe), tpe.typeSymbol)

  // Cut-n-pasted (with most original comments) and slightly adapted from
  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionSymbolOf(original: Symbol): Symbol = {
    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c
      .asInstanceOf[scala.reflect.macros.runtime.Context]
      .callsiteTyper
      .asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val owner = original.owner

    import global.analyzer.Context

    original.companion.orElse {
      import global.{abort => aabort, _}
      implicit class PatchedContext(ctx: Context) {
        trait PatchedLookupResult {
          def suchThat(criterion: Symbol => Boolean): Symbol
        }
        def patchedLookup(name: Name, expectedOwner: Symbol) =
          new PatchedLookupResult {
            override def suchThat(criterion: Symbol => Boolean): Symbol = {
              var res: Symbol = NoSymbol
              var ctx = PatchedContext.this.ctx
              while (res == NoSymbol && ctx.outer != ctx) {
                // NOTE: original implementation says `val s = ctx.scope lookup name`
                // but we can't use it, because Scope.lookup returns wrong results when the lookup is ambiguous
                // and that triggers https://github.com/scalamacros/paradise/issues/64
                val s = {
                  val lookupResult =
                    ctx.scope.lookupAll(name).filter(criterion).toList
                  lookupResult match {
                    case Nil => NoSymbol
                    case List(unique) => unique
                    case _ =>
                      aabort(
                          s"unexpected multiple results for a companion symbol lookup for $original#{$original.id}")
                  }
                }
                if (s != NoSymbol && s.owner == expectedOwner) res = s
                else ctx = ctx.outer
              }
              res
            }
          }
      }
      ctx
        .patchedLookup(original.asInstanceOf[global.Symbol].name.companionName,
                       owner.asInstanceOf[global.Symbol])
        .suchThat(sym =>
              (original.isTerm || sym.hasModuleFlag) &&
                (sym isCoDefinedWith original.asInstanceOf[global.Symbol]))
        .asInstanceOf[c.universe.Symbol]
    }
  }

  def prefix(tpe: Type): Type = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.prefix.asInstanceOf[Type]
  }

  def mkAttributedRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val sym = gTpe.typeSymbol
    global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
  }

  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gPre = pre.asInstanceOf[global.Type]
    val gSym = sym.asInstanceOf[global.Symbol]
    global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
  }

  def isNonGeneric(sym: Symbol): Boolean = {
    def check(sym: Symbol): Boolean = {
      // See https://issues.scala-lang.org/browse/SI-7424
      sym.typeSignature // force loading method's signature
      sym.annotations.foreach(_.tree.tpe) // force loading all the annotations

      sym.annotations.exists(_.tree.tpe =:= typeOf[nonGeneric])
    }

    // See https://issues.scala-lang.org/browse/SI-7561
    check(sym) ||
    (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
    sym.overrides.exists(isNonGeneric)
  }

  def isTuple(tpe: Type): Boolean =
    tpe <:< typeOf[Unit] || definitions.TupleClass.seq.contains(tpe.typeSymbol)

  def isVararg(tpe: Type): Boolean =
    tpe.typeSymbol == c.universe.definitions.RepeatedParamClass

  def devarargify(tpe: Type): Type =
    tpe match {
      case TypeRef(pre, _, args) if isVararg(tpe) =>
        appliedType(typeOf[scala.collection.Seq[_]].typeConstructor, args)
      case _ => tpe
    }

  def unByName(tpe: Type): Type =
    tpe match {
      case TypeRef(_, sym, List(tpe)) if sym == definitions.ByNameParamClass =>
        tpe
      case tpe => tpe
    }

  def equalTypes(as: List[Type], bs: List[Type]): Boolean =
    as.length == bs.length && (as zip bs).foldLeft(true) {
      case (acc, (a, b)) => acc && unByName(a) =:= unByName(b)
    }

  def alignFields(tpe: Type, ts: List[Type]): Option[List[(TermName, Type)]] = {
    val fields = fieldsOf(tpe)
    if (fields.length != ts.length) None
    else {
      @tailrec
      def loop(fields: Seq[(TermName, Type)],
               ts: Seq[Type],
               acc: List[(TermName, Type)]): Option[List[(TermName, Type)]] =
        ts match {
          case Nil => Some(acc.reverse)
          case Seq(hd, tl @ _ *) =>
            fields.span { case (_, tpe) => !(tpe =:= hd) } match {
              case (fpre, List(f, fsuff @ _ *)) =>
                loop(fpre ++ fsuff, tl, f :: acc)
              case _ => None
            }
        }

      loop(fields, ts, Nil)
    }
  }

  object HasApply {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] = {
      val sym = tpe.typeSymbol
      val companionTpe = sym.companion.info
      val applySym = companionTpe.member(TermName("apply"))
      if (applySym.isTerm && !applySym.asTerm.isOverloaded &&
          applySym.isMethod && !isNonGeneric(applySym) &&
          isAccessible(companionTpe, applySym)) {
        val applyParamss = applySym.asMethod.paramLists
        if (applyParamss.length == 1)
          alignFields(
              tpe,
              applyParamss.head.map(tpe => unByName(tpe.infoIn(companionTpe))))
        else None
      } else None
    }
  }

  object HasUnapply {
    def unapply(tpe: Type): Option[List[Type]] = {
      val sym = tpe.typeSymbol
      val companionTpe = sym.companion.info
      val unapplySym = companionTpe.member(TermName("unapply"))
      if (unapplySym.isTerm && !unapplySym.asTerm.isOverloaded &&
          unapplySym.isMethod && !isNonGeneric(unapplySym) &&
          isAccessible(companionTpe, unapplySym))
        unapplySym.asMethod
          .infoIn(companionTpe)
          .finalResultType
          .baseType(symbolOf[Option[_]]) match {
          case TypeRef(_, _, List(o @ TypeRef(_, _, args)))
              if o <:< typeOf[Product] =>
            Some(args)
          case TypeRef(_, _, args @ List(arg)) => Some(args)
          case _ => None
        } else None
    }
  }

  object HasUniqueCtor {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] =
      tpe.decls.find { sym =>
        sym.isMethod && sym.asMethod.isPrimaryConstructor &&
        isAccessible(tpe, sym)
      } match {
        case Some(ctorSym) if !isNonGeneric(ctorSym) =>
          val ctorParamss = ctorSym.asMethod.infoIn(tpe).paramLists
          if (ctorParamss.length == 1)
            alignFields(tpe, ctorParamss.head.map(param =>
                      unByName(param.info)))
          else None
        case _ => None
      }
  }

  object HasApplyUnapply {
    def apply(tpe: Type): Boolean = unapply(tpe).isDefined
    def unapply(tpe: Type): Option[List[(TermName, Type)]] =
      (tpe, tpe) match {
        case (HasApply(as), HasUnapply(bs)) if equalTypes(as.map(_._2), bs) =>
          Some(as)
        case _ => None
      }
  }

  object HasCtorUnapply {
    def apply(tpe: Type): Boolean = unapply(tpe).isDefined
    def unapply(tpe: Type): Option[List[(TermName, Type)]] =
      (tpe, tpe) match {
        case (HasUniqueCtor(as), HasUnapply(bs))
            if equalTypes(as.map(_._2), bs) =>
          Some(as)
        case _ => None
      }
  }

  trait CtorDtor {
    def construct(args: List[Tree]): Tree
    def binding: (Tree, List[Tree])
    def reprBinding: (Tree, List[Tree])
  }

  object CtorDtor {
    def apply(tpe: Type): CtorDtor = {
      val sym = tpe.typeSymbol
      val isCaseClass = sym.asClass.isCaseClass

      val repWCard = Star(Ident(termNames.WILDCARD)) // like pq"_*" except that it does work

      def narrow(tree: Tree, tpe: Type): Tree =
        tpe match {
          case ConstantType(c) =>
            q"$c.asInstanceOf[$tpe]"
          case _ =>
            tree
        }

      def narrow1(tree: Tree, tpe: Type): Tree =
        if (isVararg(tpe)) q"$tree: _*"
        else narrow(tree, tpe)

      def mkCtorDtor0(elems0: List[(TermName, Type)]) = {
        val elems = elems0.map {
          case (name, tpe) => (TermName(c.freshName("pat")), tpe)
        }
        val pattern = pq"${companionRef(tpe)}(..${elems.map {
          case (binder, tpe) =>
            if (isVararg(tpe)) pq"$binder @ $repWCard" else pq"$binder"
        }})"
        val reprPattern = elems.foldRight(q"_root_.shapeless.HNil": Tree) {
          case ((bound, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
        }
        new CtorDtor {
          def construct(args: List[Tree]): Tree =
            q"${companionRef(tpe)}(..$args)"
          def binding: (Tree, List[Tree]) =
            (pattern, elems.map {
              case (binder, tpe) => narrow(q"$binder", tpe)
            })
          def reprBinding: (Tree, List[Tree]) =
            (reprPattern, elems.map {
              case (binder, tpe) => narrow1(q"$binder", tpe)
            })
        }
      }

      def mkCtorDtor1(elems: List[(TermName, TermName, Type)],
                      pattern: Tree,
                      rhs: List[Tree]) = {
        val reprPattern = elems.foldRight(q"_root_.shapeless.HNil": Tree) {
          case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
        }
        new CtorDtor {
          def construct(args: List[Tree]): Tree = q"new $tpe(..$args)"
          def binding: (Tree, List[Tree]) = (pattern, rhs)
          def reprBinding: (Tree, List[Tree]) =
            (reprPattern, elems.map {
              case (binder, _, tpe) => narrow1(q"$binder", tpe)
            })
        }
      }

      lowerKind(tpe) match {
        // case 1: Unit
        case tpe if tpe =:= typeOf[Unit] =>
          new CtorDtor {
            def construct(args: List[Tree]): Tree = q"()"
            def binding: (Tree, List[Tree]) = (pq"()", Nil)
            def reprBinding: (Tree, List[Tree]) =
              (pq"_root_.shapeless.HNil", Nil)
          }

        // case 2: singleton
        case tpe if isCaseObjectLike(tpe.typeSymbol.asClass) =>
          val singleton = tpe match {
            case SingleType(pre, sym) =>
              c.internal.gen.mkAttributedRef(pre, sym)
            case TypeRef(pre, sym, List()) if sym.isModule =>
              c.internal.gen.mkAttributedRef(pre, sym.asModule)
            case TypeRef(pre, sym, List()) if sym.isModuleClass =>
              c.internal.gen.mkAttributedRef(pre, sym.asClass.module)
            case other =>
              abort(s"Bad case object-like type $tpe")
          }
          new CtorDtor {
            def construct(args: List[Tree]): Tree = q"$singleton: $tpe"
            def binding: (Tree, List[Tree]) = (pq"_: $tpe", Nil)
            def reprBinding: (Tree, List[Tree]) =
              (pq"_root_.shapeless.HNil", Nil)
          }

        // case 3: case class
        case tpe if isCaseClass => mkCtorDtor0(fieldsOf(tpe))

        // case 4: exactly one matching public apply/unapply
        case HasApplyUnapply(args) => mkCtorDtor0(args)

        // case 5: concrete, exactly one public constructor with matching public unapply
        case HasCtorUnapply(args) =>
          val elems = args.map {
            case (name, tpe) => (TermName(c.freshName("pat")), name, tpe)
          }
          val pattern = pq"${companionRef(tpe)}(..${elems.map {
            case (binder, _, tpe) =>
              if (isVararg(tpe)) pq"$binder @ $repWCard" else pq"$binder"
          }})"
          val rhs = elems.map {
            case (binder, _, tpe) => narrow(q"$binder", tpe)
          }
          mkCtorDtor1(elems, pattern, rhs)

        // case 6: concrete, exactly one public constructor with matching accessible fields
        case HasUniqueCtor(args) =>
          val elems = args.map {
            case (name, tpe) => (TermName(c.freshName("pat")), name, tpe)
          }
          val binder = TermName(c.freshName("pat"))
          val pattern = pq"$binder"
          val rhs = elems.map {
            case (_, name, tpe) => narrow(q"$binder.$name", tpe)
          }
          mkCtorDtor1(elems, pattern, rhs)

        case _ => abort(s"Bad product type $tpe")
      }
    }
  }
}

@macrocompat.bundle
class GenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._

  def materialize[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if (isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    if (isProduct(tpe)) mkProductGeneric(tpe)
    else mkCoproductGeneric(tpe)
  }

  def mkProductGeneric(tpe: Type): Tree = {
    val ctorDtor = CtorDtor(tpe)
    val (p, ts) = ctorDtor.binding
    val to = cq""" $p => ${mkHListValue(ts)} """
    val (rp, rts) = ctorDtor.reprBinding
    val from = cq""" $rp => ${ctorDtor.construct(rts)} """

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprTypTree(tpe)}
        def to(p: $tpe): Repr = (p match { case $to }).asInstanceOf[Repr]
        def from(p: Repr): $tpe = p match { case $from }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprTypTree(tpe)}]
    """
  }

  def mkCoproductGeneric(tpe: Type): Tree = {
    def mkCoproductCases(tpe0: Type, index: Int): CaseDef = {
      tpe0 match {
        case SingleType(pre, sym) =>
          val singleton = mkAttributedRef(pre, sym)
          cq"p if p eq $singleton => $index"
        case _ =>
          cq"_: $tpe0 => $index"
      }
    }

    val to = {
      val toCases =
        ctorsOf(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled
      q"""_root_.shapeless.Coproduct.unsafeMkCoproduct((p: @_root_.scala.unchecked) match { case ..$toCases }, p).asInstanceOf[Repr]"""
    }

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprTypTree(tpe)}
        def to(p: $tpe): Repr = $to
        def from(p: Repr): $tpe = _root_.shapeless.Coproduct.unsafeGet(p).asInstanceOf[$tpe]
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprTypTree(tpe)}]
    """
  }

  def mkIsTuple[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (!isTuple(tTpe))
      abort(s"Unable to materialize IsTuple for non-tuple type $tTpe")

    q"""new _root_.shapeless.IsTuple[$tTpe]: _root_.shapeless.IsTuple[$tTpe]"""
  }

  def mkHasProductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (isReprType(tTpe) || !isProduct(tTpe))
      abort(s"Unable to materialize HasProductGeneric for $tTpe")

    q"""new _root_.shapeless.HasProductGeneric[$tTpe]: _root_.shapeless.HasProductGeneric[$tTpe]"""
  }

  def mkHasCoproductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (isReprType(tTpe) || !isCoproduct(tTpe))
      abort(s"Unable to materialize HasCoproductGeneric for $tTpe")

    q"""new _root_.shapeless.HasCoproductGeneric[$tTpe]: _root_.shapeless.HasCoproductGeneric[$tTpe]"""
  }
}
