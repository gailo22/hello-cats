package hello.cats.json

object PhoneBook {

  trait ToJson[T] {
    def toJson(x: T): String
  }

  implicit class ToJsonSyntax[T](x: T)(implicit typeclass: ToJson[T]) {
    def toJson: String = typeclass.toJson(x)
  }

  case class Record(firstName: String, lastName: String, phone: String)

  implicit val recordToJson: ToJson[Record] = new ToJson[Record] {
    def toJson(x: Record): String = 
      s"""{"first_name": "${x.firstName}", "last_name": "${x.lastName}", "phone": "${x.phone}" }"""

  }

  implicit def listToJson[T](implicit typeclass: ToJson[T]): ToJson[List[T]] = 
    new ToJson[List[T]] {
      def toJson(x: List[T]): String =
        s"""[
          | ${x.map { e => e.toJson }.mkString(",\n ")}
          |]""".stripMargin
    }
}

import PhoneBook._

object Main2 extends App {
  val record = Record("Min", "Ho", "122334")
  println(record.toJson)

  val records = List(
    Record("James", "Ma", "3232323"),
    Record("Jun", "Me", "3232323")
  )
  println(records.toJson)
  
}