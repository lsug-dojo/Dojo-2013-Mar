
object chapter3 extends App {

  // Exercise 2
  def tail[T](l : List[T]) = l match {
    case Nil => Nil
    case _ :: tail => tail
  }

  // Exercise 3
  def drop[T](l: List[T], n: Int): List[T] = 
    (n, l) match {
      case (_, Nil) => Nil
      case (1, l) => tail(l)
      case (n, l) => drop(tail(l), n-1) 
    }

  // Exercise 4
  def dropWhile[T](l : List[T])(pred : T => Boolean) : List[T] = {
    l match {
      case Nil => Nil
      case x::xs => if(pred(x)) dropWhile(xs)(pred) else l
    }
  }

  val dropWhileForThisList = dropWhile( List(33345,4,5,3)) _

  println(dropWhileForThisList( _  > 4))

  println(tail(List(1,2, 3)))
  println(tail(List()))

  println(drop(List(), 2))
  println(drop(List(1,2, 3), 2))
  println(drop(List(1,2, 3), 4))


  println(dropWhile(List(1,2,3,4,5)) ((x: Int) => x < 3))

  def lessThanThree(n : Int) = n < 3
        

}
