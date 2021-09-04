package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList() : List[A] =
    foldRight(List[A]())(_::_)

  def take(n: Int): Stream[A] = {
    def go(n : Int, s: Stream[A]) : Stream[A] = {
      if(n <= 0) return empty
      s match {
        case Empty => empty
        case Cons(h,t) => Cons(h, () => go(n-1,t()))
      }
    }
    go(n,this)
  }

  def drop(n: Int): Stream[A] = {
    def go(n: Int, s: Stream[A]) : Stream[A] = {
      if(n <= 0) return s
      s match {
        case Empty => empty
        case Cons(_,t) => go(n-1,t())
      }
    }
    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A]) : Stream[A] = {
      s match {
        case Cons(h,t) if(p(h())) => Cons(h, () => go(t()))
        case _ => empty
      }
    }
    go(this)
  }

  def takeWhile1(p: A => Boolean) : Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) Cons(() => a, () => b.takeWhile1(p)) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None : Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B) : Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean) : Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) cons(a, b.filter(p)) else b.filter(p))

  def append[B>:A](s: Stream[B]) : Stream[B] =
    foldRight(empty[B])((a,b) => if(b == empty) cons(a, s) else cons(a, b))

  def flapMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a).headOption.get, b))

  def startsWith[B](s: Stream[B]): Boolean = ???

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3).take(2).toList())
    println(Stream(1,2,3).drop(2).toList())
    println(Stream(1,2,3).takeWhile1(_%2 == 0).toList())
    println(Stream(1,2,3).takeWhile1(_%2 == 1).toList())
    println(Stream(1,2,3).headOption)
    println(empty.headOption)
    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)

    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs(n : Int) : Stream[Int] = ???


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}