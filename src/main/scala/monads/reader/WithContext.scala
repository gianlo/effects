package monads.reader

final case class WithContext[Context, Result](runWith: Context => Result)

object WithContext {
  def constant[Context, Result](value: => Result): WithContext[Context, Result] = WithContext(_ => value)
  def threadContext[Context, Result, Output](wc: WithContext[Context, Result])(nextStep: Result => WithContext[Context, Output]): WithContext[Context, Output] = WithContext(
    c => {
      val result = wc.runWith(c)
      nextStep(result).runWith(c)
    }
  )

  implicit class WithContextOps[A, B](w: WithContext[A, B]) {
    def flatMap[C](f: B => WithContext[A, C]): WithContext[A, C] = threadContext(w)(f)

    def map[C](f: B => C): WithContext[A, C] = WithContext(c => {
      val result = w.runWith(c)
      f(result)
    })
  }
}

final case class Person(name: String)
final case class Address(street: String)

trait PeopleDB {
  def find(id: String): Person
  def findAddress(person: Person): Address
}

object App {

  def retrieveStreet(personId: String): WithContext[PeopleDB, String] = for {
    person <- WithContext((db: PeopleDB) => db.find(personId))
    address <- WithContext((db: PeopleDB) => db.findAddress(person))
  } yield address.street

  def main(args: Array[String]): Unit = {

    val concreteDb = new PeopleDB {
      override def find(id: String): Person = {
        println(s"retrieve person $id")
        Person("ciccio")
      }

      override def findAddress(person: Person): Address = {
        println(s"retrieve address for ${person.name}")
        Address("pappo street")
      }
    }

    val idToRetrieve = args.headOption.getOrElse("U123")
    println(retrieveStreet(idToRetrieve).runWith(concreteDb))
  }
}
