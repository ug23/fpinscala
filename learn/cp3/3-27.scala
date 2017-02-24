sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def count[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + count(l) + count(r)
  }

  def maximum(tree: Tree[Int]):Int = tree match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // 葉まで数える => 葉までいくまで1を足していく => 左右でデカイ方を採用
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l,r) => (depth(l) max depth(l)) + 1
  }

  def map[A,B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l,f),map(r,f))
  }

  // 抽象化…？
  def fold[A,B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(x) => 
  }

}


val b = Branch(Branch(Leaf(10), Leaf(20)),Branch(Leaf(50), Leaf(2)))
println("count: "+Tree.count(b))

println("maximum: "+Tree.maximum(b))
def double(x: Int): Int = x*2
println("double: "+Tree.map(b,double))

