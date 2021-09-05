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
    def go(n : Int, s: Stream[A]) : Stream[A] = s match {
      case Cons(h,t) if n > 0 => Cons(h, () => go(n-1,t()))
      case _ => empty
    }
    go(n,this)
  }

  def drop(n: Int): Stream[A] = {
    def go(n: Int, s: Stream[A]) : Stream[A] = s match {
      case s@Cons(_,t) => if(n > 0) go(n-1,t()) else s
      case _ => empty
    }
    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A]) : Stream[A] = s match {
      case Cons(h,t) if p(h()) => Cons(h, () => go(t()))
      case _ => empty
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

  def map_1[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take_1(n: Int): Stream[A] = unfold((this, n))(z => {
    z._1 match {
      case Cons(h,t) if z._2 > 0 => Some((h(), (t(), z._2-1)))
      case _ => None
    }
  })

  def takeWhile_1(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](sb: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, sb)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()),Some(h2())), (t1(),t2())))
    case (Cons(h1,t1), Empty) => Some(( (Some(h1()), None), (t1(), empty) ))
    case (Empty, Cons(h2,t2)) => Some(( (None, Some(h2())), (empty, t2()) ))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).foldRight(true)( (o,v) => o match {
      case (Some(a), Some(b)) => if(a == b) v else false
      case (_, None) => true
      case (None, _) => false
    } )

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some((s, t()))
    case _ => None
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h,t) => {
      val tail = t().scanRight(z)(f)
      cons(f(h(), tail.headOption.get), tail)
    }
  }

  def scanRight_1[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight(Stream(z))((a,s) => cons(f(a,s.headOption.get), s))
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

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

  val ones_1: Stream[Int] = unfold(1)(s => Some(s,s))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def constant_1[A](a: A): Stream[A] =
    unfold(a)(s => Some(s,s))

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def from_1(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s+1)))

  def fibs() : Stream[Int] = {
    def go(x: Int, y: Int) : Stream[Int] =
      cons(x+y, go(y,x+y))
    cons(0,cons(1, go(0,1)))
  }

  def fibs_1() : Stream[Int] =
    unfold((0,1))(s => Some(s._1, (s._2, s._1 + s._2)))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((x,y)) => cons(x, unfold(y)(f))
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3).take(2).toList())
    println(Stream(1,2,3).drop(2).toList())
    println(Stream(1,2,3).takeWhile_1(_%2 == 0).toList())
    println(Stream(1,2,3).takeWhile_1(_%2 == 1).toList())
    println(Stream(1,2,3).headOption)
    println(empty.headOption)
    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)

    println(ones.take(5).toList)
    println(ones_1.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
    println(fibs().take(10).toList())
    println(fibs_1().take(10).toList())
    println(from(11).take(10).toList())
    println(from_1(11).take(10).toList())
    println(constant(11).take_1(10).toList())
    println(constant_1(11).take_1(10).toList())
    println(ones.map_1(_ => 2).take_1(10).toList())

    println(ones.take(3).zipWith(constant(0).take(2))(_+_).toList())

    println(Stream(1,2,3).startsWith(Stream(1,2)))
    println(Stream(1,2).startsWith(Empty))
    println(Empty.startsWith(Empty))

    println(Stream(1,2).startsWith(Stream(1,2,3)))
    println(Empty.startsWith(Stream(1)))

    println(Stream(1,2,3).hasSubsequence(Stream(2,3)))
    println(Stream(1,2,3).hasSubsequence(Empty))
    println(Empty.hasSubsequence(Empty))
    println(Empty.hasSubsequence(ones.take(1)))

    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
    println(Stream(1,2,3).scanRight_1(0)(_ + _).toList)
  }
}