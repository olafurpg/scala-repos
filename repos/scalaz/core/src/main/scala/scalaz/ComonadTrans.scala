package scalaz

trait ComonadTrans[F[_ [_], _]]
  def lower[G[_]: Cobind, A](a: F[G, A]): G[A]

object ComonadTrans
  def apply[F[_ [_], _]](implicit F: ComonadTrans[F]): ComonadTrans[F] = F

trait Cohoist[F[_ [_], _]] extends ComonadTrans[F]
  def cohoist[M[_], N[_]: Comonad](f: M ~> N): F[M, ?] ~> F[N, ?]

object Cohoist
  def apply[F[_ [_], _]](implicit F: Cohoist[F]): Cohoist[F] = F
