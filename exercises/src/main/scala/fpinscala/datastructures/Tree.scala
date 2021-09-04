package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t : Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size (r)
    }
  }

  def maximum(t : Tree[Int]) : Int = {
    t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l) max maximum(r)
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(merge: (B, B) => B) : B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => merge(fold(l)(f)(merge), fold(r)(f)(merge))
    }
  }

  def size2[A](t : Tree[A]) : Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximum2(t : Tree[Int]) : Int = {
    fold(t)(x => x)((x, y) => x max y)
  }

  def map_2[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)) : Tree[B])(Branch(_,_))
  }
}