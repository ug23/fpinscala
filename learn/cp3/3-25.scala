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


}


val b = Branch(Branch(Leaf("a"), Leaf("b")),Branch(Leaf("c"), Leaf("d")))
println(Tree.count(b))