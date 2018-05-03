package Base
import scala.io.Source
import scala.collection.mutable.ListBuffer

object main {
  def main(args: Array[String]){

    println("Problem 1: ")
    problem1;

    println("Problem 2: ")
    problem2;

    println("Problem 3: ")
    problem3;

    println("Problem 4: ")
    problem4;

    println("Problem 5: ")
    problem5;

    println("Problem 6: ")
    problem6;

    println("Problem 7: ")
    problem7;

    println("Problem 8: ")
    problem8;

  }

  def problem1: Unit =
  {

    println("Input List[List[Int]]: ")

    val p1intInput = scala.io.StdIn.readLine()
    println(p1intInput)

    println("Input List[List[String]]: ")

    val p1stringInput = scala.io.StdIn.readLine()
    println(p1stringInput)

    //(list (list foo bar baz) (list foo foo foo))
    val integer: List[List[Int]] = SExprParser.parseList[List[Int]]( p1intInput ).get
    val integerList = indirectMap[Int, Int](x => x*2, integer)


    val string : List[List[String]] = SExprParser.parseList[List[String]]( p1stringInput ).get
    val stringList = indirectMap[String, String](_.reverse, string)


    println(SExprPrinter.print[List[List[Int]]](integerList))
    println(SExprPrinter.print[List[List[String]]](stringList))

  }

  def problem2: Unit =
  {
    println("Input List[String]:");

    val p2in1 = scala.io.StdIn.readLine()
    println(p2in1)

    println("Input List[Int]:")

    val p2in2 = scala.io.StdIn.readLine()
    println(p2in2)

    //(list 1 2 3 4)
    //(list foo bar baz)
    val listA: List[String] = SExprParser.parseList[String]( p2in1 ).get
    val listB: List[Int] = SExprParser.parseList[Int]( p2in2 ).get


    val xform = zip[String, Int](listA, listB)
    println(SExprPrinter.print[List[(String, Int)]](xform))

  }

  def problem3: Unit =
  {
    println("Input List[String]: ")
    val p3in1 = scala.io.StdIn.readLine()
    println(p3in1)


    val aList: List[String] = SExprParser.parseList[String]( p3in1 ).get
    val flatList = flatMap[String,Char](splitList, aList)


    println("Input List[Int]: ")
    val p3in2 = scala.io.StdIn.readLine()
    println(p3in2)


    //(list 1 2 5)
    val fList: List[Int] = SExprParser.parseList[Int]( p3in2 ).get
    val fiboList = flatMap[Int, Int]( fibonacci , fList )

    println(SExprPrinter.print[List[Char]]( flatList ))
    println(SExprPrinter.print[List[Int]]( fiboList ))

  }

  def problem4: Unit =
  {

    println("Input: Rope[Int]");
    val p4in1 = scala.io.StdIn.readLine()
    println(p4in1);

    println("Input: Rope[String]")
    val p4in2 = scala.io.StdIn.readLine()
    println(p4in2);

    val p4ropeInt = SExprParser.parseRope[Int](p4in1).get
    val p4ropeStr = SExprParser.parseRope[String](p4in2).get

    println(SExprPrinter.print[Rope[Int]]( mapRope[Int]( double , p4ropeInt ) ))
    println(SExprPrinter.print[Rope[String]]( mapRope[String]( reverse , p4ropeStr ) ))

  }

  def problem5: Unit =
  {
    println("Input: Rope");

    val p5in1 = scala.io.StdIn.readLine()
    println(p5in1)
    val p5ropeInt = SExprParser.parseRope[Int]( p5in1 ).get
    println(foldRope[Int](p5ropeInt))

  }

  def problem6: Unit =
  {
    println("Input: Rope")

    val p6in1 = scala.io.StdIn.readLine()
    println(p6in1)

    val p6ropeInt = SExprParser.parseRope[Int]( p6in1 ).get
    println(ropeLength[Int](p6ropeInt))

  }

  def problem7: Unit =
  {
    println("Input: Rope")

    val p7in = scala.io.StdIn.readLine()
    println(p7in)

    val p7ropeInt = SExprParser.parseRope[Int]( p7in ).get
    println(SExprPrinter.print[List[Int]](ropeToList[Int](p7ropeInt)))

  }

  def problem8: Unit =
  {
    println("Input String to be reduced: ");

    val sentence = scala.io.StdIn.readLine()
    println(sentence);

    val reduced = mapReduce[String,Int,String, Int](map, reduce, sentence);
    reduced.map(a => println(a._1 + ": " + a._2));


  }

  /*
 *
 *
  HELPER FUNCTIONS
  *
  *
  */

  /*
   PROBLEM 1
  */

  def indirectMap[A, B](function: A => B, listA: List[List[A]] ) =
  {
    listA.map(_.map(function(_)))
  }


  /*
   PROBLEM 2
  */

  def zip[A, B](listA: List[A], listB: List[B] ) : List[(A,B)]=
  {
    if(listA.size > listB.size)
      listA.slice(0,listB.size)
    else
      listB.slice(0,listA.size)

    listA.indices.map(i => ( listA(i) , listB(i) ) ).toList
  }

  /*
   PROBLEM 3
  */

  def flatMap[A,B](function: A => List[B], listA : List[A] ) : List[B] =
  {
    listA.map(function(_)).foldLeft(List[B]())((a, b) => a ::: b)
  }

  def splitList(str : String) : List[Char] =
  {
    str.toList
  }

  def fibonacci(num : Int) : List[Int] =
  {
    fibHelp(1,1).take(num+1).toList
  }

  def fibHelp(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibHelp(b, a + b))


  /*
  PROBLEM 4
   */
  def mapRope[A](function : A => A, rope : Rope[A]): Rope[A] = rope match
  {
    case RConcat(a,b) =>
    {
      RConcat(mapRope(function, a), mapRope(function, b))
    }
    case RList(list) =>
    {
      RList(list.map(function(_)))
    }
  }

  def double(x : Int) : Int =
  {
    x*2
  }

  def reverse(x : String) : String =
  {
    x.reverse
  }


  /*
  PROBLEM 5
   */
  def foldRope[A](rope : Rope[A] ) : String = rope match
  {
    case RConcat(a,b) => foldRope(a) + foldRope(b)
    case RList(list) => list.mkString("")
  }

  /*
PROBLEM 6
 */
  def ropeLength[A](rope : Rope[A] ) : Int = rope match
  {
    case RConcat(a,b) => ropeLength(a) + ropeLength(b)
    case RList(list) => list.foldLeft(0)((sum,_) => sum + 1 )
  }

  /*
PROBLEM 7
 */

  def ropeToList[A](rope : Rope[A] ) : List[A] = rope match
  {
    case RConcat(a,b) => ropeToList(a) ::: ropeToList(b)
    case RList(list) => list
  }

  /*
PROBLEM 8
*/

  def mapReduce[A,B,C,D](f1: A => List[(C , D)] , f2: ((C, List[D])) => (C, D), r1 : A ) : List[(C, D)] =
  {

    val listC = f1(r1).par.groupBy(_._1).mapValues(_.map(_._2)).toList;

    val listD = listC.map(a => (a._1 , a._2.toList))

    listD.map(f2(_))
  }

  def map(str : String) : List[(String, Int)] =
  {
    val listA = str.split(" ").toList
    listA.map((_, 1))
  }

  def reduce(tuple : (String, List[Int]) ) : (String, Int) =
  {
    ( tuple._1 , tuple._2.foldLeft(0)(_ + _))
  }
}





