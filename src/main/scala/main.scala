package Base

import scala.io.Source

object Main {
  def main(argv: Array[String]) {
    /*SExprParser.parseRope[Int]("(+ (list 1 2 3 4 5) (+ (list 6) (list 7)))") match {
      case Some(matched) => println(matched)
      case _ => println("no good")*/


    /*val filename = "/Users/nickkpoon/IdeaProjects/a_3/src/main/scala/input.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString
    val xs: List[List[Int]] = SExprParser.parseList[List[Int]](fileContents).get
    println(xs)*/

    val filename = "/Users/nickkpoon/IdeaProjects/a_3/src/main/scala/input.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString
    //val xs: List[List[Int]] = SExprParser.parseList[List[Int]](fileContents).get
    //val xss = xs.flatten
    //val xsss = xss.map{ _ * 2 }

    val stringList: List[List[String]] = SExprParser.parseList[List[String]](fileContents).get
    val singlyStringList = stringList.flatten
    //val function1 = indirectMap(xsss)
    val stringFunction = indirectMap(singlyStringList)

    val stringListSize = stringList.size
    val stringListLength = stringList.length

    //val smallStringList: List[String] = SExprParser.parseList[String](stringList)

    println("the size of the string list is: " + stringListSize)
    println("the length of the string list is: " + stringListLength)
    println(stringList)
    println(singlyStringList)
    println(stringFunction)

    println("printing for each")

    //print out elements inside
    stringList.foreach{println}
    println("printing in specific element")
    println(stringList(1))


    //QUESTION 1 PART 1 COMPLETE!!!
    println("\n\nworking function ! ! !")

    stringList.foreach{x => println(indirectMap(x))}
  }


  //def indirectMap(A:List[Int] => List[Int]) : Int =
  //{}

  /*def indirectMap(a: List[Int]) =
    a.map{_ * 2}*/

  def indirectMap(f: List[String]) =
    f.map(reverse)

  /*def indirectMap(a: List[Either[String,Int]]): Unit =
  {
    a match
    {
      case List[Int] =>
    }

  }*/

  def reverse(s: String): String =
  {
    s.reverse
  }

  //def zip (A:Int => Int)
 }
