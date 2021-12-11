trait SemigroupN[A] {
  def combine(x: A, y: A): A // combine must be associative
}

trait Monoid[A] extends SemigroupN[A] {
  // empty must be an identity element (0 in addition)
  def empty: A
}

trait FunctorN[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait SemigroupalN[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

trait ApplyN[F[_]] extends SemigroupalN[F] with FunctorN[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
}

trait ApplicativeN[F[_]] extends ApplyN[F] {
  def pure[A](a: A): F[A]
}

trait MonadN[F[_]] extends ApplicativeN[F] {
//  def pure[A](value: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
}
