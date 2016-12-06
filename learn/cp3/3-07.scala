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

}

// まずは例題を動かしてみた
val l = List(1,2,3,4,5)

println(List.sum2(l))
println(List.product2(l))

/*
************************
不可能。
foldRightは末尾に到達するまでfの引数が評価されないので、途中で中断することができない。
もし途中で止めるなら、fに例外をはくような機構が必要…？思いつかない

************************
*/