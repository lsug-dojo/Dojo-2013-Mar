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

  println(tail(List(1,2, 3)))
  println(tail(List()))

  println(drop(List(), 2))
  println(drop(List(1,2, 3), 2))
  println(drop(List(1,2, 3), 4))


  println(dropWhile(List(1,2,3,4,5)) ((x: Int) => x < 3))

  def lessThanThree(n : Int) = n < 3
  
  // Exercise 11
  def foldLeft[A,B] (l: List[A], z: B)(f: (B,A) => B): B = {
    l match {
      case Nil => z
      case x :: xs => foldLeft( xs, f(z, x))(f)
    }
  }
  
  // Exercise 13
  def reverse[T](l: List[T]): List[T] = {
    foldLeft(l, Nil : List[T])( (acc, x) => x :: acc)
  }
  
  // Exercise 14
  def foldLeft2[A,B](l: List[A], z:B)(f: (B,A) =>B): B = {
    def ident(x: B): B = x
    val g = l.foldRight(ident _)((e, a) => x => a(f(x, e)))
    g(z)
  }
          

/////////////////////////////
// May Dojo

// Exercise 17
  def addOne(l: List[Int]) : List[Int] = {
    l match {
      case Nil => Nil
      case x::xs => (x+1) :: addOne(xs)
    }
  }

  println(addOne(List(1,2,3)))

// Exercise 18
  def toString (l:List[Double]):String = {
    l match {
      case Nil => ""
      case x::xs => x.toString+toString(xs)
    }
  }

  // Exercise 19
  println(toString(List(1.0, 3.6,7)))

  def map[A,B](l:List[A]) (f:A =>B):List[B] = {
    l match {
      case Nil => Nil
      case x::xs => f(x) :: map(xs)(f)
    }
  }
  
  println(map(List(1,2,3))( x => x.toString + "bla"))

  // Exercise 20
  def filter[A] (l:List[A]) (pred: A => Boolean):List[A] ={
   l match {
     case Nil => Nil
     case x::xs => if (pred(x)) x :: filter(xs)(pred) else filter(xs)(pred)
   }
  }

  println(filter(List(1,2,3,4))( x => x % 2 == 0))
  
  
  // Exercise 21
  def flatMap[A,B] (l : List[A])(f : A => List[B]) : List[B] = {
    l match {
      case Nil => Nil
      case x::xs => f(x) ::: flatMap(xs)(f)
    }
  }

  println(map(List(1,2,3))( x => List(x,x)))
  println(flatMap(List(1,2,3))( x => List(x,x)))

  def listPred[A](pred: A => Boolean)( x : A) : List[A] = if(pred(x)) List(x) else Nil

  // Exercise 22
  def filter2[A](l: List[A])(pred: A => Boolean) : List[A] = flatMap(l) ( x => if(pred(x)) List(x) else Nil)

  def filter3[A](l: List[A])(pred: A => Boolean) : List[A] = flatMap(l) (listPred(pred) )

  println(filter2(List(1,2,3,4))( x => x % 2 == 0))
  println(filter3(List(1,2,3,4))( x => x % 2 == 0))

  // Exercise 23
  def addTwoLists (l1:List[Int], l2:List[Int]): List[Int] = {
  
    (l1,l2) match {
      case (Nil,l2) => l2
      case (l1,Nil) =>l1
      case (x::xs,y::ys) => (x+y)::addTwoLists(xs,ys)
    }
  }

  println(addTwoLists(List(1,2,3,4), List(4,5,6)))

  // Exercise 24
  def zipTwoLists[A](l1: List[A], l2: List[A])( f: (A,A) => A):List[A] = {
     
    (l1,l2) match {
      case (Nil,l2) => l2
      case (l1,Nil) =>l1
      case (x::xs,y::ys) => f(x,y)::zipTwoLists(xs,ys)(f)
    }
  }

  println(zipTwoLists(List(1,2,3,4), List(4,5,6))( _+_))
  println(zipTwoLists(List("t","i"), List("i","s"))( _+_))

 
  // Exercise 26
  sealed trait Tree[+A]

  case class Leaf[A] (value: A) extends Tree[A]
  case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t:Tree[A]): Int = {
    t match {
      case Leaf(x) => 1
      case Branch(r,l) => 1 + size(r) + size(l)
}
    }

  val tree = Branch(Leaf(1), Leaf(2))
  println(size(tree))

  //Exercise 27
  def max(t: Tree[Int]) : Int = {
    t match {
      case Leaf(x) => x
      case Branch(r,l) => max(r).max(max(l))
    }
  }
  val treeMax = Branch(Branch(Leaf(4),Leaf(1)),Leaf(2))
  println(max(treeMax))
}
