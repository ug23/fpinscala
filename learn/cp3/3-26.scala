sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Leafは1
  // Branchはその枝葉

  def count[A](tree: Tree[A]): Int = tree match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + count(l) + count(r)
  }

  def maximum(tree: Tree[Int]):Int = tree match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

}


val b = Branch(Branch(Leaf(10), Leaf(20)),Branch(Leaf(50), Leaf(2)))
println(Tree.maximum(b))