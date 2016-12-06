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

  // def tail[A](list: List[A]): List[A] = list match {
  //   case Nil => sys.error("This list is Nil.")
  //   case Cons(_, tail) => tail
  // }

  def drop[A](l: List[A], n: Int): List[A]= l match {
    case Nil => sys.error("This list is Nil.")
    case Cons(h,t) if n>0 => drop(t, n-1)
    case Cons(h,t) => Cons(h,t)
  }

}

val l = List(1,2,3,4)
println(List.drop(l, 2))
println(List.drop(l, 3))
println(List.drop(l, 6))

