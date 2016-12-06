sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Int]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Int]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((x,_) => x+1)
  }
}

val list0 = List()
val list1 = List(1,2,3)
val list2 = List(1,5,4,8,2,8)

println(s"${List.sum2(list0)}: ${List.sum3(list0)}")
println(s"${List.sum2(list1)}: ${List.sum3(list1)}")
println(s"${List.sum2(list2)}: ${List.sum3(list2)}")

println(s"${List.product2(list0)}: ${List.product3(list0)}")
println(s"${List.product2(list1)}: ${List.product3(list1)}")
println(s"${List.product2(list2)}: ${List.product3(list2)}")

println(s"${List.length(list0)}: ${List.length2(list0)}")
println(s"${List.length(list1)}: ${List.length2(list1)}")
println(s"${List.length(list2)}: ${List.length2(list2)}")

