package monads

trait FunctionK[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}
