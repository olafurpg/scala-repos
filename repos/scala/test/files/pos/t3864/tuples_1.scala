trait EnrichedType[X] {
  val value: X
}

trait Tuples {

  trait Tuple15W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
      extends EnrichedType[
          Tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]] {
    def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z): Z = {
      import value._;
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple15[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(
          _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)
    }
    def mapElements[
        AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO](
        _1: (A => AA) = identity[A] _,
        _2: (B => BB) = identity[B] _,
        _3: (C => CC) = identity[C] _,
        _4: (D => DD) = identity[D] _,
        _5: (E => EE) = identity[E] _,
        _6: (F => FF) = identity[F] _,
        _7: (G => GG) = identity[G] _,
        _8: (H => HH) = identity[H] _,
        _9: (I => II) = identity[I] _,
        _10: (J => JJ) = identity[J] _,
        _11: (K => KK) = identity[K] _,
        _12: (L => LL) = identity[L] _,
        _13: (M => MM) = identity[M] _,
        _14: (N => NN) = identity[N] _,
        _15: (O => OO) = identity[O] _)
      : (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15))
  }

  implicit def ToTuple15W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N,
      O)): Tuple15W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] =
    new { val value = t }
    with Tuple15W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

  trait Tuple16W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
      extends EnrichedType[
          Tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]] {
    def fold[Z](
        f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z): Z = {
      import value._;
      f(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple16[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z]): IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16)
    }
    def mapElements[
        AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP](
        _1: (A => AA) = identity[A] _,
        _2: (B => BB) = identity[B] _,
        _3: (C => CC) = identity[C] _,
        _4: (D => DD) = identity[D] _,
        _5: (E => EE) = identity[E] _,
        _6: (F => FF) = identity[F] _,
        _7: (G => GG) = identity[G] _,
        _8: (H => HH) = identity[H] _,
        _9: (I => II) = identity[I] _,
        _10: (J => JJ) = identity[J] _,
        _11: (K => KK) = identity[K] _,
        _12: (L => LL) = identity[L] _,
        _13: (M => MM) = identity[M] _,
        _14: (N => NN) = identity[N] _,
        _15: (O => OO) = identity[O] _,
        _16: (P => PP) = identity[P] _)
      : (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16))
  }

  implicit def ToTuple16W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
      P)): Tuple16W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] =
    new { val value = t }
    with Tuple16W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]

  trait Tuple17W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
      extends EnrichedType[
          Tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]] {
    def fold[Z](
        f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple17[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17)
    }
    def mapElements[
        AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ](
        _1: (A => AA) = identity[A] _,
        _2: (B => BB) = identity[B] _,
        _3: (C => CC) = identity[C] _,
        _4: (D => DD) = identity[D] _,
        _5: (E => EE) = identity[E] _,
        _6: (F => FF) = identity[F] _,
        _7: (G => GG) = identity[G] _,
        _8: (H => HH) = identity[H] _,
        _9: (I => II) = identity[I] _,
        _10: (J => JJ) = identity[J] _,
        _11: (K => KK) = identity[K] _,
        _12: (L => LL) = identity[L] _,
        _13: (M => MM) = identity[M] _,
        _14: (N => NN) = identity[N] _,
        _15: (O => OO) = identity[O] _,
        _16: (P => PP) = identity[P] _,
        _17: (Q => QQ) = identity[Q] _)
      : (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17))
  }

  implicit def ToTuple17W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
      Q)): Tuple17W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] =
    new { val value = t }
    with Tuple17W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]

  trait Tuple18W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
      extends EnrichedType[
          Tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]] {
    def fold[Z](
        f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
        R) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17,
        _18)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple18[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17,
                 _18)
    }
    def mapElements[AA,
                    BB,
                    CC,
                    DD,
                    EE,
                    FF,
                    GG,
                    HH,
                    II,
                    JJ,
                    KK,
                    LL,
                    MM,
                    NN,
                    OO,
                    PP,
                    QQ,
                    RR](_1: (A => AA) = identity[A] _,
                        _2: (B => BB) = identity[B] _,
                        _3: (C => CC) = identity[C] _,
                        _4: (D => DD) = identity[D] _,
                        _5: (E => EE) = identity[E] _,
                        _6: (F => FF) = identity[F] _,
                        _7: (G => GG) = identity[G] _,
                        _8: (H => HH) = identity[H] _,
                        _9: (I => II) = identity[I] _,
                        _10: (J => JJ) = identity[J] _,
                        _11: (K => KK) = identity[K] _,
                        _12: (L => LL) = identity[L] _,
                        _13: (M => MM) = identity[M] _,
                        _14: (N => NN) = identity[N] _,
                        _15: (O => OO) = identity[O] _,
                        _16: (P => PP) = identity[P] _,
                        _17: (Q => QQ) = identity[Q] _,
                        _18: (R => RR) = identity[R] _): (AA, BB, CC,
    DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17),
       _18(value._18))
  }

  implicit def ToTuple18W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
      R)): Tuple18W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] =
    new { val value = t }
    with Tuple18W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]

  trait Tuple19W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
      extends EnrichedType[
          Tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]] {
    def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R,
        S) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17,
        _18,
        _19)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple19[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17,
                 _18,
                 _19)
    }
    def mapElements[AA,
                    BB,
                    CC,
                    DD,
                    EE,
                    FF,
                    GG,
                    HH,
                    II,
                    JJ,
                    KK,
                    LL,
                    MM,
                    NN,
                    OO,
                    PP,
                    QQ,
                    RR,
                    SS](_1: (A => AA) = identity[A] _,
                        _2: (B => BB) = identity[B] _,
                        _3: (C => CC) = identity[C] _,
                        _4: (D => DD) = identity[D] _,
                        _5: (E => EE) = identity[E] _,
                        _6: (F => FF) = identity[F] _,
                        _7: (G => GG) = identity[G] _,
                        _8: (H => HH) = identity[H] _,
                        _9: (I => II) = identity[I] _,
                        _10: (J => JJ) = identity[J] _,
                        _11: (K => KK) = identity[K] _,
                        _12: (L => LL) = identity[L] _,
                        _13: (M => MM) = identity[M] _,
                        _14: (N => NN) = identity[N] _,
                        _15: (O => OO) = identity[O] _,
                        _16: (P => PP) = identity[P] _,
                        _17: (Q => QQ) = identity[Q] _,
                        _18: (R => RR) = identity[R] _,
                        _19: (S => SS) = identity[S] _): (AA, BB, CC, DD,
    EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17),
       _18(value._18),
       _19(value._19))
  }

  implicit def ToTuple19W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R,
      S)): Tuple19W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] =
    new { val value = t }
    with Tuple19W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]

  trait Tuple20W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
      extends EnrichedType[Tuple20[
              A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]] {
    def fold[Z](f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S,
        T) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17,
        _18,
        _19,
        _20)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple20[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17,
                 _18,
                 _19,
                 _20)
    }
    def mapElements[AA,
                    BB,
                    CC,
                    DD,
                    EE,
                    FF,
                    GG,
                    HH,
                    II,
                    JJ,
                    KK,
                    LL,
                    MM,
                    NN,
                    OO,
                    PP,
                    QQ,
                    RR,
                    SS,
                    TT](_1: (A => AA) = identity[A] _,
                        _2: (B => BB) = identity[B] _,
                        _3: (C => CC) = identity[C] _,
                        _4: (D => DD) = identity[D] _,
                        _5: (E => EE) = identity[E] _,
                        _6: (F => FF) = identity[F] _,
                        _7: (G => GG) = identity[G] _,
                        _8: (H => HH) = identity[H] _,
                        _9: (I => II) = identity[I] _,
                        _10: (J => JJ) = identity[J] _,
                        _11: (K => KK) = identity[K] _,
                        _12: (L => LL) = identity[L] _,
                        _13: (M => MM) = identity[M] _,
                        _14: (N => NN) = identity[N] _,
                        _15: (O => OO) = identity[O] _,
                        _16: (P => PP) = identity[P] _,
                        _17: (Q => QQ) = identity[Q] _,
                        _18: (R => RR) = identity[R] _,
                        _19: (S => SS) = identity[S] _,
                        _20: (T => TT) = identity[T] _): (AA, BB,
    CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17),
       _18(value._18),
       _19(value._19),
       _20(value._20))
  }

  implicit def ToTuple20W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))
    : Tuple20W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] =
    new { val value = t }
    with Tuple20W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]

  trait Tuple21W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
      extends EnrichedType[Tuple21[
              A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]] {
    def fold[Z](
        f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T,
        U) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17,
        _18,
        _19,
        _20,
        _21)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple21[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17,
                 _18,
                 _19,
                 _20,
                 _21)
    }
    def mapElements[AA,
                    BB,
                    CC,
                    DD,
                    EE,
                    FF,
                    GG,
                    HH,
                    II,
                    JJ,
                    KK,
                    LL,
                    MM,
                    NN,
                    OO,
                    PP,
                    QQ,
                    RR,
                    SS,
                    TT,
                    UU](_1: (A => AA) = identity[A] _,
                        _2: (B => BB) = identity[B] _,
                        _3: (C => CC) = identity[C] _,
                        _4: (D => DD) = identity[D] _,
                        _5: (E => EE) = identity[E] _,
                        _6: (F => FF) = identity[F] _,
                        _7: (G => GG) = identity[G] _,
                        _8: (H => HH) = identity[H] _,
                        _9: (I => II) = identity[I] _,
                        _10: (J => JJ) = identity[J] _,
                        _11: (K => KK) = identity[K] _,
                        _12: (L => LL) = identity[L] _,
                        _13: (M => MM) = identity[M] _,
                        _14: (N => NN) = identity[N] _,
                        _15: (O => OO) = identity[O] _,
                        _16: (P => PP) = identity[P] _,
                        _17: (Q => QQ) = identity[Q] _,
                        _18: (R => RR) = identity[R] _,
                        _19: (S => SS) = identity[S] _,
                        _20: (T => TT) = identity[T] _,
                        _21: (U => UU) = identity[U] _): (AA, BB, CC, DD, EE,
    FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17),
       _18(value._18),
       _19(value._19),
       _20(value._20),
       _21(value._21))
  }

  implicit def ToTuple21W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))
    : Tuple21W[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] =
    new { val value = t } with Tuple21W[
        A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]

  trait Tuple22W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
      extends EnrichedType[Tuple22[A,
                                   B,
                                   C,
                                   D,
                                   E,
                                   F,
                                   G,
                                   H,
                                   I,
                                   J,
                                   K,
                                   L,
                                   M,
                                   N,
                                   O,
                                   P,
                                   Q,
                                   R,
                                   S,
                                   T,
                                   U,
                                   V]] {
    def fold[Z](
        f: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U,
        V) => Z): Z = {
      import value._;
      f(_1,
        _2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        _10,
        _11,
        _12,
        _13,
        _14,
        _15,
        _16,
        _17,
        _18,
        _19,
        _20,
        _21,
        _22)
    }
    def toIndexedSeq[Z](implicit ev: value.type <:< Tuple22[
            Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z])
      : IndexedSeq[Z] = {
      val zs = ev(value); import zs._;
      IndexedSeq(_1,
                 _2,
                 _3,
                 _4,
                 _5,
                 _6,
                 _7,
                 _8,
                 _9,
                 _10,
                 _11,
                 _12,
                 _13,
                 _14,
                 _15,
                 _16,
                 _17,
                 _18,
                 _19,
                 _20,
                 _21,
                 _22)
    }
    def mapElements[AA,
                    BB,
                    CC,
                    DD,
                    EE,
                    FF,
                    GG,
                    HH,
                    II,
                    JJ,
                    KK,
                    LL,
                    MM,
                    NN,
                    OO,
                    PP,
                    QQ,
                    RR,
                    SS,
                    TT,
                    UU,
                    VV](_1: (A => AA) = identity[A] _,
                        _2: (B => BB) = identity[B] _,
                        _3: (C => CC) = identity[C] _,
                        _4: (D => DD) = identity[D] _,
                        _5: (E => EE) = identity[E] _,
                        _6: (F => FF) = identity[F] _,
                        _7: (G => GG) = identity[G] _,
                        _8: (H => HH) = identity[H] _,
                        _9: (I => II) = identity[I] _,
                        _10: (J => JJ) = identity[J] _,
                        _11: (K => KK) = identity[K] _,
                        _12: (L => LL) = identity[L] _,
                        _13: (M => MM) = identity[M] _,
                        _14: (N => NN) = identity[N] _,
                        _15: (O => OO) = identity[O] _,
                        _16: (P => PP) = identity[P] _,
                        _17: (Q => QQ) = identity[Q] _,
                        _18: (R => RR) = identity[R] _,
                        _19: (S => SS) = identity[S] _,
                        _20: (T => TT) = identity[T] _,
                        _21: (U => UU) = identity[U] _,
                        _22: (V => VV) = identity[V] _): (AA, BB, CC, DD, EE,
    FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU, VV) =
      (_1(value._1),
       _2(value._2),
       _3(value._3),
       _4(value._4),
       _5(value._5),
       _6(value._6),
       _7(value._7),
       _8(value._8),
       _9(value._9),
       _10(value._10),
       _11(value._11),
       _12(value._12),
       _13(value._13),
       _14(value._14),
       _15(value._15),
       _16(value._16),
       _17(value._17),
       _18(value._18),
       _19(value._19),
       _20(value._20),
       _21(value._21),
       _22(value._22))
  }

  implicit def ToTuple22W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
      t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U,
      V)): Tuple22W[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] =
    new { val value = t } with Tuple22W[
        A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
}
