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


  def dropWhile[A](l: List[A], f: A=> Boolean):List[A] = {
    def sub(list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(h, t) if f(h) => sub(t)
      case Cons(h, t) if !f(h) => t
    }
    sub(l)
  }
}

val l = List(1,2,3,4,10,5,6,7,8,9)
println(List.dropWhile(l, (i:Int)=>i<10))
println(List.dropWhile(Nil, (i:Int)=>i<10))
