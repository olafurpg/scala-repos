object Test {
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String,
      String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U,
      V)*): Tuple22[
      A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String,
      String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)*)
    : Tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] =
    null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String,
      String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)*)
    : Tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] =
    null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String,
      String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R,
      S)*): Tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] =
    null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String,
      String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
      R)*): Tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] =
    null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
      Q)*): Tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
      P)*): Tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M, N,
      O)*): Tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L, M,
      N)*): Tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L, M](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K, L,
      M)*): Tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K, L](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J, K,
      L)*): Tuple12[A, B, C, D, E, F, G, H, I, J, K, L] = null
  def foo[A, B, C, D, E, F, G, H, I, J, K](
      heading: (String, String, String, String, String, String, String, String,
      String, String, String),
      rows: (A, B, C, D, E, F, G, H, I, J,
      K)*): Tuple11[A, B, C, D, E, F, G, H, I, J, K] = null
  def foo[A, B, C, D, E, F, G, H, I, J](
      heading: (String, String, String, String, String, String, String, String,
      String, String),
      rows: (A, B, C, D, E, F, G, H, I,
      J)*): Tuple10[A, B, C, D, E, F, G, H, I, J] = null
  def foo[A, B, C, D, E, F, G, H, I](
      heading: (String, String, String, String, String, String, String, String,
      String),
      rows: (A, B, C, D, E, F, G, H, I)*): Tuple9[A, B, C, D, E, F, G, H, I] =
    null
  def foo[A, B, C, D, E, F, G, H](heading: (String, String, String, String,
                                  String, String, String, String),
                                  rows: (A, B, C, D, E, F, G,
                                  H)*): Tuple8[A, B, C, D, E, F, G, H] = null
  def foo[A, B, C, D, E, F, G](
      heading: (String, String, String, String, String, String, String),
      rows: (A, B, C, D, E, F, G)*): Tuple7[A, B, C, D, E, F, G] = null
  def foo[A, B, C, D, E, F](
      heading: (String, String, String, String, String, String),
      rows: (A, B, C, D, E, F)*): Tuple6[A, B, C, D, E, F] = null
  def foo[A, B, C, D, E](heading: (String, String, String, String, String),
                         rows: (A, B, C, D, E)*): Tuple5[A, B, C, D, E] = null
  def foo[A, B, C, D](heading: (String, String, String, String),
                      rows: (A, B, C, D)*): Tuple4[A, B, C, D] = null
  def foo[A, B, C](
      heading: (String, String, String), rows: (A, B, C)*): Tuple3[A, B, C] =
    null
  def foo[A, B](heading: (String, String), rows: (A, B)*): Tuple2[A, B] = null
  def foo[A](heading: String, rows: A*): Tuple1[A] = null

  foo((1))
}
