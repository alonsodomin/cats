package cats
package data

case class StoreT[F[_], S, A](runF: F[S => A], pos: S) {
  def peek(s: S)(implicit F: Comonad[F]): A =
    peeks(_ => s)

  def peeks(f: S => S)(implicit F: Comonad[F]): A =
    F.extract(runF)(f(pos))

  def seek(s: S): StoreT[F, S, A] = seeks(_ => s)

  def seeks(f: S => S): StoreT[F, S, A] = StoreT(runF, f(pos))

  def experiment[G[_]](f: S => G[S])(implicit F: Comonad[F], G: Functor[G]): G[A] =
    G.map(f(pos))(F.extract(runF))
}

object StoreT {

  implicit def storeTComonad[F[_], S](implicit F: Comonad[F]): Comonad[StoreT[F, S, ?]] = new Comonad[StoreT[F, S, ?]] {
    def extract[A](fa: StoreT[F, S, A]): A =
      F.extract(fa.runF)(fa.pos)

    def coflatMap[A, B](fa: StoreT[F, S, A])(f: StoreT[F, S, A] => B): StoreT[F, S, B] =
      StoreT(F.coflatMap(fa.runF)(ff => s => f(StoreT(ff, s))), fa.pos)

    def map[A, B](fa: StoreT[F, S, A])(f: A => B): StoreT[F, S, B] =
      StoreT(F.map(fa.runF)(f compose _), fa.pos)
  }

}