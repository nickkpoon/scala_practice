package Base
import scala.util.parsing.combinator._

// You shouldn't care much about what this object does, 
// the only methods you should care about are:
// - parseList[T](s: String): List[T]
// - parseRope[T](s: String): Rope[T]
// Where T can be any of the following:
// - Int
// - String
// - Boolean
// - (A, B) where A and B are in this list of types
// - List[A] where A is in this list of types
// - Rope[A] where A is in this list of types
object SExprParser extends RegexParsers {
  trait Parsable[T] {
    def parser: Parser[T]
  }

  implicit object intInstance extends Parsable[Int] {
    def parser = """(?:\+|\-)?[0-9]""".r ^^ { case s => s.toInt }
  }

  implicit object boolInstance extends Parsable[Boolean] {
    def parser = """true|false""".r ^^ {
      case "true"  => true
      case "false" => false
    }
  }

  implicit object stringInstance extends Parsable[String] {
    def parser = """[A-Za-z0-9]+""".r
  }

  implicit object charInstance extends Parsable[Char] {
    def parser = """[A-Za-z0-9]""".r ^^ (_.head)
  }

  implicit def tupleInstance[A : Parsable, B : Parsable]: Parsable[(A, B)] = new Parsable[(A, B)] {
    def parser = implicitly[Parsable[A]].parser ~ implicitly[Parsable[B]].parser ^^ { case a ~ b => (a, b) }
  }

  implicit def listInstance[A : Parsable]: Parsable[List[A]] = new Parsable[List[A]] {
    def parser = list(implicitly[Parsable[A]])
  }

  implicit def ropeInstance[A : Parsable]: Parsable[Rope[A]] = new Parsable[Rope[A]] {
    def parser = rope(implicitly[Parsable[A]])
  }

  def ident: Parser[String] = """[A-Za-z][A-Za-z0-9]*""".r
  def command[T](cmd: String, body: Parser[T]): Parser[T] =
    "(" ~> cmd ~>! body <~ ")"

  def list[T](implicit p: Parsable[T]): Parser[List[T]] =
    command("list", rep(p.parser))

  def rope[T](implicit p: Parsable[T]): Parser[Rope[T]] = {
    list(p) ^^ { case x => RList(x) } | command("+", rope(p) ~ rope(p) ^^ { case x ~ y => RConcat(x, y) })
  }

  def parseList[T](str: String)(implicit p: Parsable[T]): Option[List[T]] = {
    parse(list[T], str) match {
      case Success(x, _) => Some(x)
      case _ => None
    }
  }

  def parseRope[T](str: String)(implicit p: Parsable[T]): Option[Rope[T]] = {
    parse(rope[T], str) match {
      case Success(x, _) => Some(x)
      case _ => None
    }
  }

}

object SExprPrinter {
  trait Printable[T] {
    val print: T => String
  }

  implicit object intInstance extends Printable[Int] {
    //val print = _.toString
    val print = _.toString
  }
  implicit object boolInstance extends Printable[Boolean] {
    //val print = _.toString
    val print = _.toString
  }
  implicit object charInstance extends Printable[Char] {
    //val print = _.toString
    val print = _.toString
  }
  implicit object stringInstance extends Printable[String] {
    val print = identity
  }
  implicit def tupleInstance[A:Printable, B:Printable]: Printable[(A, B)] = new Printable[(A,B)] {
    val print = { case (x, y) => implicitly[Printable[A]].print(x) + " " + implicitly[Printable[B]].print(y) }
  }
  implicit def listInstance[A:Printable]: Printable[List[A]] = new Printable[List[A]] {
    val print = (x) => x.map(y => implicitly[Printable[A]].print(y)).mkString("(list ", " ", ")")
  }
  implicit def ropeInstance[A:Printable]: Printable[Rope[A]] = new Printable[Rope[A]] {
    val print = {
      case RList(list) => listInstance[A].print(list)
      case RConcat(l, r) => List(ropeInstance[A].print(l), ropeInstance[A].print(r)).mkString("(+ ", " ", ")")
    }
  }

  def print[T](x: T)(implicit p: Printable[T]) = p.print(x)
}

// This is your rope datatype.
// It is either a list (RList constructor)
// or a concatenation of two ropes (RConcat constructor)
sealed trait Rope[T]
case class RList[T](l: List[T]) extends Rope[T]
case class RConcat[T](l: Rope[T], r: Rope[T]) extends Rope[T]

// A function that only serves as an example on how to use ropes
object Rope {
  def add[T](x: T, xs: Rope[T]): Rope[T] = xs match {
    case RList(list)   => RList(x :: list)
    case RConcat(a, b) => RConcat(add(x, a), b)
  }
}

// Example of how to use parseRope and parseList:
// object Main {
//   def main(argv: Array[String]) {
//     SExprParser.parseRope[Int]("(+ (list 1 2 3 4 5) (+ (list 6) (list 7)))") match {
//       case Some(matched) => println(matched)
//       case _ => println("no good")
//     }

//   }
// }

// Alternatively:
// object Main {
//   def main(argv: Array[String]) {
//     // Reads a nested list
//     val xs: List[List[Int]] = SExprParser.parseList[List[Int]](scala.io.StdIn.readLine()).get
//     println(xs)
//   }
// }