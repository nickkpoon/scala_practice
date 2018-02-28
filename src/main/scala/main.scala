package Base
import scala.io.Source
import scala.collection.mutable.ListBuffer


object Main {
  def main(argv: Array[String]) {
    /*SExprParser.parseRope[Int]("(+ (list 1 2 3 4 5) (+ (list 6) (list 7)))") match {
      case Some(matched) => println(matched)
      case _ => println("no good")*/


    /*val filename = "/Users/nickkpoon/IdeaProjects/a_3/src/main/scala/input.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString
    val xs: List[List[Int]] = SExprParser.parseList[List[Int]](fileContents).get
    println(xs)*/


    /*val smallStringList: List[String] = SExprParser.parseList[String](stringList)


      println("the size of the string list is: " + stringListSize)
      println("the length of the string list is: " + stringListLength)
      println(stringList)
      println(singlyStringList)
      println(stringFunction)

      */
    //print out elements inside
    /* println("printing for each")
     stringList.foreach{println}

     println("printing in specific element")
     println(stringList(1))*/

    val filename = "/Users/nickkpoon/IdeaProjects/a_3/src/main/scala/input.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString

    //val xs: List[List[Int]] = SExprParser.parseList[List[Int]](fileContents).get
    //val xss = xs.flatten
    //val xsss = xss.map{ _ * 2 }

    val stringList: List[List[String]] = SExprParser.parseList[List[String]](fileContents).get

    /*println(stringList.mkString)
    val singlyStringList = stringList.flatten

    println("printing list . . .")
    println(stringList)


    println("printing first element from list . . .")
    val firstElem: String = singlyStringList(0)


    if(isNumeric(getFirstElem(flattenNestedList(stringList))))
    println("YEYE IT iS")
    else
      println("NAH")
    //val stringFunction = indirectMap(singlyStringList)

    val stringListSize = stringList.size
    val stringListLength = stringList.length



    //QUESTION 1 PART 1 COMPLETE!!!
    println("\n\nworking function ! ! !")

    //stringList.foreach{x => println(indirectMap(x))}

    //pickList(stringList)*/

    indirectMap(getFirstElem(flattenNestedList(stringList)))




  }

  def flattenNestedList(a:List[List[String]]) : List[String] =
  {
    a.flatten
  }

  def getFirstElem(a:List[String]) : String =
  {
    a(0)
  }

  def indirectMap(a: String) : Unit =
  {
    val filename = "/Users/nickkpoon/IdeaProjects/a_3/src/main/scala/input.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString


    if(isNumeric(a))
      {
        val intList: List[List[Int]] = SExprParser.parseList[List[Int]](fileContents).get
        //intList.foreach{x => println(indirectMapInt(x))}
        handleInt(intList)
      }
    else
      {
        val stringList: List[List[String]] = SExprParser.parseList[List[String]](fileContents).get
        //stringList.foreach{x => println(indirectMapString(x))}
        handleString(stringList)
      }
  }

  def handleInt(a:List[List[Int]]) : Unit =
  {
    val intBuffer = new ListBuffer[List[Int]]
    a.foreach{x => intBuffer+=(indirectMapInt(x))}

    println(intBuffer.toList)

  }

  def handleString(a:List[List[String]]) : Unit =
  {
    val StringBuffer = new ListBuffer[List[String]]
    a.foreach{x => StringBuffer+=(indirectMapString(x))}

    println(StringBuffer.toList)

  }


  def pickList(a: Any) = a match
    {
    case a: List[String] => println("ISSA STRING LIST")
    case a: List[Int] => println("ISSA INT LIST")
  }

  def indirectMapString(f: List[String]) : List[String] =
    f.map(reverse)

  def indirectMapInt(f: List[Int]) : List[Int] =
    f.map{_*2}

  def matchTypes(a: Any) = a match
  {
    case a: String => reverse(a)
    case a: Int => double(a)
  }
  //helper function for reversing strings within list
  def reverse(s: String): String =
  {
    s.reverse
  }

  def double(i: Int): Int =
  {
    i * 2
  }


  //helper function to check if content is numeric
  def isNumeric(input: String): Boolean = input.forall(_.isDigit)




  //def zip (A:Int => Int)
 }






