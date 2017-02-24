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

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((l, a) => Cons(a,l))
  }

  def append[A](base:List[A], l: List[A]):List[A] = {
    foldRight(l,base)(Cons(_,_))
  }

  def concat[A](a:List[List[A]]):List[A] = {
    foldRight(a,List[A]())((l,b) => append(l,b))
  }

  def add1(list:List[Int]):List[Int] = {
    foldRight(list,List[Int]())((a,b)=>Cons(a+1,b))
  }

  def double2String(list:List[Double]):List[String] = {
    foldRight(list,List[String]())((d,t)=>Cons(d.toString,t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
      foldRight(l,List[B]())((d,t)=>Cons(f(d),t))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l,List[A]())((h,t)=>if(f(h)) Cons(h,t) else t)
  }
}

val list0 = List()
val list1 = List(1,2,3,4,5,6)
val list2 = List(1.0,2.5,3.333)

// println(List.add1(list1))
// println(List.double2String(list2))
// println(List.map(list2)(_.toString))

println(List.filter(list1)(_ % 2 == 0))