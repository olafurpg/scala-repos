/*
 * Copyright (c) 2013-16 Miles Sabin
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

import scala.language.dynamics
import scala.language.existentials
import scala.language.experimental.macros

import scala.reflect.macros.{blackbox, whitebox}

import tag.@@
import scala.util.Try

trait Witness extends Serializable {
  type T
  val value: T {}
}

object Witness extends Dynamic {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }

  implicit def apply[T]: Witness.Aux[T] =
    macro SingletonTypeMacros.materializeImpl[T]

  implicit def apply[T](t: T): Witness.Lt[T] =
    macro SingletonTypeMacros.convertImpl

  def mkWitness[T0](value0: T0): Aux[T0] =
    new Witness {
      type T = T0
      val value = value0
    }

  implicit val witness0: Witness.Aux[_0] = new Witness {
    type T = _0
    val value = Nat._0
  }

  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    new Witness {
      type T = Succ[P]
      val value = new Succ[P]()
    }

  def selectDynamic(tpeSelector: String): Any =
    macro SingletonTypeMacros.witnessTypeImpl
}

trait WitnessWith[TC[_]] extends Witness {
  val instance: TC[T]
}

trait LowPriorityWitnessWith {
  implicit def apply2[H, TC2[_ <: H, _], S <: H, T](
      t: T): WitnessWith.Lt[({ type λ[X] = TC2[S, X] })#λ, T] =
    macro SingletonTypeMacros.convertInstanceImpl2[H, TC2, S]
}

object WitnessWith extends LowPriorityWitnessWith {
  type Aux[TC[_], T0] = WitnessWith[TC] { type T = T0 }
  type Lt[TC[_], Lub] = WitnessWith[TC] { type T <: Lub }

  implicit def apply1[TC[_], T](t: T): WitnessWith.Lt[TC, T] =
    macro SingletonTypeMacros.convertInstanceImpl1[TC]
}

trait NatWith[TC[_ <: Nat]] {
  type N <: Nat

  val instance: TC[N]
}

object NatWith {
  type Aux[TC[_ <: Nat], N0 <: Nat] = NatWith[TC] { type N = N0 }

  implicit def apply[TC[_ <: Nat]](i: Any): NatWith[TC] =
    macro SingletonTypeMacros.convertInstanceImplNat[TC]

  implicit def apply2[B, T <: B, TC[_ <: B, _ <: Nat]](
      i: Int): NatWith[({ type λ[t <: Nat] = TC[T, t] })#λ] =
    macro SingletonTypeMacros.convertInstanceImplNat1[B, T, TC]
}

/**
  * Provides the widen type of a singleton type.
  *
  * Type member `Out` of an implicitly available `Witness[T]` instance is the widen type
  * of `T`, and the `apply` method explicitly converts a `T` to an `Out`.
  *
  * E.g. if `T` is ``Witness.`2`.T``, `Out` is `Int`.
  *
  * It somehow complements `Witness`, providing the corresponding non-witnessed standard type, if any.
  *
  * Example of use,
  * {{
  *   val w = Widen[Witness.`2`.T]
  *   // w.Out is Int
  *   // w(2) is typed as Int
  * }}
  *
  * @author Alexandre Archambault
  */
trait Widen[T] extends DepFn1[T] { type Out >: T }

object Widen {
  def apply[T](implicit widen: Widen[T]): Aux[T, widen.Out] = widen

  type Aux[T, Out0 >: T] = Widen[T] { type Out = Out0 }

  def instance[T, Out0 >: T](f: T => Out0): Aux[T, Out0] =
    new Widen[T] {
      type Out = Out0
      def apply(t: T) = f(t)
    }

  implicit def apply1[TC[_], T](t: T): WitnessWith.Lt[TC, T] =
    macro SingletonTypeMacros.convertInstanceImpl1[TC]

  implicit def materialize[T, Out]: Aux[T, Out] =
    macro SingletonTypeMacros.materializeWiden[T, Out]
}

@macrocompat.bundle
trait SingletonTypeUtils extends ReprTypes {
  import c.universe.{Try => _, _}
  import internal.decorators._

  def singletonOpsTpe = typeOf[syntax.SingletonOps]
  val SymTpe = typeOf[scala.Symbol]

  object LiteralSymbol {
    def unapply(t: Tree): Option[String] = t match {
      case q""" scala.Symbol.apply(${Literal(Constant(s: String))}) """ =>
        Some(s)
      case _ => None
    }
  }

  object SingletonSymbolType {
    val atatTpe = typeOf[@@[_, _]].typeConstructor
    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def apply(s: String): Type =
      appliedType(atatTpe, List(SymTpe, c.internal.constantType(Constant(s))))

    def unapply(t: Type): Option[String] =
      t match {
        case RefinedType(
            List(
              SymTpe,
              TypeRef(_, TaggedSym, List(ConstantType(Constant(s: String))))),
            _) =>
          Some(s)
        case _ => None
      }
  }

  def mkSingletonSymbol(s: String): Tree = {
    val sTpe = SingletonSymbolType(s)
    q"""_root_.scala.Symbol($s).asInstanceOf[$sTpe]"""
  }

  object SingletonType {
    def unapply(t: Tree): Option[Type] = (t, t.tpe) match {
      case (Literal(k: Constant), _) => Some(c.internal.constantType(k))
      case (LiteralSymbol(s), _) => Some(SingletonSymbolType(s))
      case (_, keyType @ SingleType(p, v))
          if !v.isParameter && !isValueClass(v) =>
        Some(keyType)
      case (q""" $sops.narrow """, _) if sops.tpe <:< singletonOpsTpe =>
        Some(sops.tpe.member(TypeName("T")).typeSignature)
      case _ => None
    }
  }

  def narrowValue(t: Tree): (Type, Tree) = {
    t match {
      case Literal(k: Constant) =>
        val tpe = c.internal.constantType(k)
        (tpe, q"$t.asInstanceOf[$tpe]")
      case LiteralSymbol(s) => (SingletonSymbolType(s), mkSingletonSymbol(s))
      case _ => (t.tpe, t)
    }
  }

  def parseLiteralType(typeStr: String): Option[Type] =
    for {
      parsed <- Try(c.parse(typeStr)).toOption
      checked = c.typecheck(parsed, silent = true) if checked.nonEmpty
      tpe <- SingletonType.unapply(checked)
    } yield tpe

  def parseStandardType(typeStr: String): Option[Type] =
    for {
      parsed <- Try(c.parse(s"null.asInstanceOf[$typeStr]")).toOption
      checked = c.typecheck(parsed, silent = true) if checked.nonEmpty
    } yield checked.tpe

  def parseType(typeStr: String): Option[Type] =
    parseStandardType(typeStr).orElse(parseLiteralType(typeStr))

  def typeCarrier(tpe: Type) =
    mkTypeCarrier(tq"{ type T = $tpe }")

  def fieldTypeCarrier(tpe: Type) =
    mkTypeCarrier(
      tq"{ type T = $tpe ; type ->>[V] = Field[V] ; type Field[V] = _root_.shapeless.labelled.FieldType[$tpe,V] }")

  def mkTypeCarrier(tree: Tree) = {
    val carrier = c.typecheck(tree, mode = c.TYPEmode).tpe

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    Literal(Constant(())).setType(carrier)
  }

  def isValueClass(sym: Symbol): Boolean = {
    val tSym = sym.typeSignature.typeSymbol
    tSym.isClass && tSym.asClass.isDerivedValueClass
  }
}

@macrocompat.bundle
class SingletonTypeMacros(val c: whitebox.Context)
    extends SingletonTypeUtils
    with NatMacroDefns {
  import c.universe._
  import internal.decorators._

  def mkWitness(sTpe: Type, s: Tree): Tree = {
    q"""
      _root_.shapeless.Witness.mkWitness[$sTpe]($s.asInstanceOf[$sTpe])
    """
  }

  def mkWitnessWith(parent: Type, sTpe: Type, s: Tree, i: Tree): Tree = {
    val name = TypeName(c.freshName("anon$"))
    val iTpe = i.tpe.finalResultType

    q"""
      {
        final class $name extends $parent {
          val instance: $iTpe = $i
          type T = $sTpe
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def mkWitnessNat(parent: Type, sTpe: Type, s: Tree, i: Tree): Tree = {
    val name = TypeName(c.freshName("anon$"))
    val iTpe = i.tpe.finalResultType

    q"""
      {
        final class $name extends $parent {
          val instance: $iTpe = $i
          type N = $sTpe
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def mkOps(sTpe: Type, w: Tree): Tree = {
    val name = TypeName(c.freshName("anon$"))

    q"""
      {
        final class $name extends _root_.shapeless.syntax.SingletonOps {
          type T = $sTpe
          val witness = $w
        }
        new $name
      }
    """
  }

  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gPre = pre.asInstanceOf[global.Type]
    val gSym = sym.asInstanceOf[global.Symbol]
    global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
  }

  def extractSingletonValue(tpe: Type): Tree =
    tpe match {
      case ConstantType(c: Constant) => Literal(c)

      case SingleType(p, v) if !v.isParameter && !isValueClass(v) =>
        mkAttributedRef(p, v)

      case SingletonSymbolType(c) => mkSingletonSymbol(c)

      case _ =>
        c.abort(c.enclosingPosition,
                s"Type argument $tpe is not a singleton type")
    }

  def materializeImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    mkWitness(tpe, extractSingletonValue(tpe))
  }

  def extractResult(t: Tree)(mkResult: (Type, Tree) => Tree): Tree =
    (t.tpe, t) match {
      case (tpe @ ConstantType(c: Constant), _) =>
        mkResult(tpe, Literal(c))

      case (tpe @ SingleType(p, v), tree)
          if !v.isParameter && !isValueClass(v) =>
        mkResult(tpe, tree)

      case (SymTpe, LiteralSymbol(s)) =>
        mkResult(SingletonSymbolType(s), mkSingletonSymbol(s))

      case (tpe, tree)
          if tree.symbol.isTerm && tree.symbol.asTerm.isStable &&
            !isValueClass(tree.symbol) =>
        val sym = tree.symbol.asTerm
        val pre =
          if (sym.owner.isClass) c.internal.thisType(sym.owner) else NoPrefix
        val symTpe = c.internal.singleType(pre, sym)
        mkResult(symTpe, q"$sym.asInstanceOf[$symTpe]")

      case _ =>
        c.abort(
          c.enclosingPosition,
          s"Expression $t does not evaluate to a constant or a stable reference value")
    }

  def convertImpl(t: Tree): Tree = extractResult(t)(mkWitness)

  def inferInstance(tci: Type): Tree = {
    val i = c.inferImplicitValue(tci)
    if (i == EmptyTree)
      c.abort(c.enclosingPosition,
              s"Unable to resolve implicit value of type $tci")
    i
  }

  def convertInstanceImplNat[TC[_ <: Nat]](i: Tree)(
      implicit tcTag: WeakTypeTag[TC[Nothing]]): Tree =
    convertInstanceImplNatAux(i, tcTag.tpe)

  def convertInstanceImplNat1[B, T <: B, TC[_ <: B, _ <: Nat]](i: Tree)(
      implicit tTag: WeakTypeTag[T],
      tcTag: WeakTypeTag[TC[Nothing, Nothing]]): Tree = {
    val tTpe = tTag.tpe
    val tc = tcTag.tpe.typeConstructor
    val tcParam = tc.typeParams(1)
    val tcTpe = c.internal.polyType(
      List(tcParam),
      appliedType(tc, List(tTpe, tcParam.asType.toType)))
    convertInstanceImplNatAux(i, tcTpe)
  }

  def convertInstanceImplNatAux(i: Tree, tcTpe: Type): Tree = {
    val (n, nTpe) = i match {
      case NatLiteral(n) => (mkNatValue(n), mkNatTpe(n))
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"Expression $i does not evaluate to a non-negative Int literal")
    }
    val nwTC = typeOf[NatWith[Any]].typeConstructor
    val parent = appliedType(nwTC, List(tcTpe))
    val instTpe = appliedType(tcTpe, List(nTpe))
    val iInst = inferInstance(instTpe)
    mkWitnessNat(parent, nTpe, n, iInst)
  }

  def convertInstanceImpl1[TC[_]](t: Tree)(
      implicit tcTag: WeakTypeTag[TC[_]]): Tree =
    extractResult(t) { (sTpe, value) =>
      val tc = tcTag.tpe.typeConstructor
      val wwTC = typeOf[WitnessWith[Nothing]].typeConstructor
      val parent = appliedType(wwTC, List(tc))
      val tci = appliedType(tc, List(sTpe))
      val i = inferInstance(tci)
      mkWitnessWith(parent, sTpe, value, i)
    }

  def convertInstanceImpl2[H, TC2[_ <: H, _], S <: H](t: Tree)(
      implicit tc2Tag: WeakTypeTag[TC2[_, _]],
      sTag: WeakTypeTag[S]): Tree =
    extractResult(t) { (sTpe, value) =>
      val tc2 = tc2Tag.tpe.typeConstructor
      val s = sTag.tpe

      val parent = weakTypeOf[WitnessWith[({ type λ[X] = TC2[S, X] })#λ]].map {
        case TypeRef(prefix, sym, args) if sym.isFreeType =>
          internal.typeRef(NoPrefix, tc2.typeSymbol, args)
        case tpe => tpe
      }

      val tci = appliedType(tc2, List(s, sTpe))
      val i = inferInstance(tci)
      mkWitnessWith(parent, sTpe, value, i)
    }

  def mkSingletonOps(t: Tree): Tree =
    extractResult(t) { (tpe, tree) =>
      mkOps(tpe, mkWitness(tpe, tree))
    }

  def narrowSymbol[S <: String: WeakTypeTag](t: Tree): Tree = {
    (weakTypeOf[S], t) match {
      case (ConstantType(Constant(s1: String)), LiteralSymbol(s2))
          if s1 == s2 =>
        mkSingletonSymbol(s1)
      case _ =>
        c.abort(c.enclosingPosition,
                s"Expression $t is not an appropriate Symbol literal")
    }
  }

  def witnessTypeImpl(tpeSelector: Tree): Tree = {
    val q"${tpeString: String}" = tpeSelector
    val tpe = parseLiteralType(tpeString).getOrElse(
      c.abort(c.enclosingPosition, s"Malformed literal $tpeString"))

    fieldTypeCarrier(tpe)
  }

  def materializeWiden[T: WeakTypeTag, Out: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias

    val widenTpe = tpe match {
      case SingletonSymbolType(s) => symbolTpe
      case _ => tpe.widen
    }

    if (widenTpe =:= tpe)
      c.abort(c.enclosingPosition, s"Don't know how to widen $tpe")
    else q"_root_.shapeless.Widen.instance[$tpe, $widenTpe](x => x)"
  }
}
