sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]): Int = ints match { 
    case Nil => 0 
    case Cons(x,xs) => x + sum(xs) 
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs,z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns,0)(_+_)
  def product2(ns: List[Int]) = foldRight(ns,1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as,0)((_,y) => 1 + y)
  }
}

val list0 = List()
val list1 = List(1)
val list9 = List(81,81,81,81,81,81,81,81,81)

println(List.length(list0))
println(List.length(list1))
println(List.length(list9))
