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

  // そのままNilで扱うパターン
  def setHead[A](head:A, list: List[A]): List[A] = list match {
    case Nil => List(head)
    case Cons(_, tail) => Cons(head, tail)
  }

  // Nilが来ていることを例外でアラートするパターン
  def setHead2[A](head:A, list: List[A]): List[A] = list match {
    case Nil => sys.error("This list is Nil.")
    case Cons(_, tail) => Cons(head, tail)
  }
}

val l = List(1,2,3,4)
println(List.setHead(0,l))
println(List.setHead(0,Nil))
println()
println(List.setHead2(0,l))
println(List.setHead2(0,Nil))
