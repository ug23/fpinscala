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

  // ほかにはtailがNilになるまで走査して、その間に出てきたheadを別のListに足していく方法もある
  // どちらにせよ、次の要素へのポインタを持つ単方向リストなので、最後の要素を操作するには確実にO(n)かかる
  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("This list is Nil.")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  
}

val l = List(1,2,3,4)
println(List.init(l))
println(List.init(Nil))