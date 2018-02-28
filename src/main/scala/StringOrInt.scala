class StringOrInt[T]
object StringOrInt
{
  implicit object IntWitness extends StringOrInt[Int]
  implicit object StringWitness extends StringOrInt[String]
}


object Bar
{
  def foo[T: StringOrInt](x: T) = x match
  {
    case _: String => println("str")
    case _: Int => println("int")
  }

}