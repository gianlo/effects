package monads.free

import monads.{FunctionK, Functor}
import monads.Id.Id

sealed trait ConsoleIO[T]
final case class ReadLn[T](next: String => T) extends ConsoleIO[T]
final case class WriteLn[T](content: String, next: T) extends ConsoleIO[T]

object ConsoleIO {
  implicit val functorInstance: Functor[ConsoleIO] = new Functor[ConsoleIO] {
    override def map[A, B](fa: ConsoleIO[A])(f: A => B): ConsoleIO[B] = fa match {
      case ReadLn(next) => ReadLn(x => f(next(x)))
      case WriteLn(content, next) => WriteLn(content, f(next))
    }
  }

  type ConsoleFree[T] = Free[ConsoleIO, T]

  def readLn: ConsoleFree[String] = Free.liftF(ReadLn(identity))

  def writeLn(content: String): ConsoleFree[Unit] = Free.liftF(WriteLn(content, ()))
}

object ConsoleIOExample {
  import ConsoleIO._

  val prog: Free[ConsoleIO, String] = for {
    _ <- writeLn("Please input name:")
    name <- readLn
    _ <- writeLn(s"Hello $name!")
  } yield name

  val plainInterpreter: FunctionK[ConsoleIO, Id] = new FunctionK[ConsoleIO, Id]{
    override def apply[T](a: ConsoleIO[T]): Id[T] = a match {
      case ReadLn(next) => next(scala.io.StdIn.readLine())
      case WriteLn(txt, next) =>
        println(txt)
        next
    }
  }

  def main(args: Array[String]): Unit = {
    import monads.Id._

    val name: Id[String] = prog.foldMap(plainInterpreter)
    println(s"Bound value: $name")
  }

}




