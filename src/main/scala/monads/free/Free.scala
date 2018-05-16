package monads.free

import monads.{FunctionK, Functor, Monad}

import scala.language.higherKinds

sealed trait Free[F[_], T]
final case class Pure[F[_], T](value: T) extends Free[F, T]
final case class Impure[F[_], T](next: F[Free[F,T]]) extends Free[F, T]

object Free{
  def monadInstance[F[_]](implicit ff: Functor[F]): Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    override def pure[A](a: A): Free[F, A] = Pure(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa match {
      case Pure(a) => f(a)
      case Impure(pause) => Impure[F, B](ff.map[Free[F,A], Free[F,B]](pause)(xa => flatMap(xa)(f)))
    }
  }

  implicit class FreeOps[F[_], A](fa: Free[F, A])(implicit ff: Functor[F]){
    def flatMap[B](f: A => Free[F, B]): Free[F,B] = monadInstance.flatMap(fa)(f)
    def map[B](f: A => B): Free[F,B] = monadInstance.map(fa)(f)
    def foldMap[G[_]](eta: FunctionK[F, G])(implicit mg: Monad[G]): G[A] = fa match {
      case Pure(a) => mg.pure(a)
      case Impure(pause) => mg.flatMap(eta(pause))(gg => gg.foldMap(eta))
    }
  }

  def liftF[F[_], A](fa: F[A])(implicit ff: Functor[F]): Free[F, A] = Impure(ff.map(fa)(a => Pure(a)))
}
