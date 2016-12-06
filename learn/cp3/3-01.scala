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

}

val x = List(1,2,3,4,5) match {
  // Cons(1, Cons(2, Cons(3,_))) になるため不適
  case Cons(x, Cons(2, Cons(4, _))) => x
  // Cons(1,_) のため不適
  case Nil => 42
  // Cons(1, Cons(2, Cons(3, Cons(4, _)))) となりここに該当
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  // Cons(1, Cons(2,_)) のため該当するがひとつ前にヒットするためここにはこない
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}


println(x)