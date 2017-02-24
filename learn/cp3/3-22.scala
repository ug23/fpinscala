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
    foldRight(base,l)(Cons(_,_))
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

  def flatMap[A,B](as:List[A])(f: A=>List[B]):List[B] = {
    concat(map(as)(f))
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => 
      if(f(a)) List(a) 
      else List())
  }

  // Intのみ有効
  def addWith2List(a: List[Int], b: List[Int]):List[Int]={
    (a,b) match {
      case (Cons(h,t), Cons(i,u)) => Cons(h+i, addWith2List(t, u))
      case _ => Nil
    }
  }
}

val list1:List[Int] = List(1,2,3)
val list2:List[Int] = List(4,5,6)

println(List.addWith2List(list1,list2))
