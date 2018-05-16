package monads

trait Monad[F[_]] extends Functor[F]{
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap[A, B](fa)(a => pure(f(a)))
  def join[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)
}
