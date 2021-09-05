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

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if(n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // remember right parameter of foldRight is the folded value
  def takeWhile1(p: A => Boolean) : Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None : Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B) : Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  // remember right parameter of foldRight is folded value
  def filter(p: A => Boolean) : Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else b)

  // use appended value as default value directly
  // unnecessary to check default value and replace it with s at the right end
  def append[B>:A](s: Stream[B]) : Stream[B] =
    foldRight(s)((a,b) => cons(a, b))

  // use append to concat the single value stream and tail stream
  // rather than use head option to get the single value and
  // use the smart constructor to assemble single value and tail stream
  def flapMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  def map_1[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  // use tuple directly for pattern matching
  def take_1(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h,t), n) if n > 0 => Some((h(), (t(), n-1)))
    case _ => None
  }

  def takeWhile_1(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](sb: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, sb)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  }

  // it seems that this is not a exercise in the book
  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()),Some(h2())), (t1(),t2())))
    case (Cons(h1,t1), Empty) => Some(( (Some(h1()), None), (t1(), empty) ))
    case (Empty, Cons(h2,t2)) => Some(( (None, Some(h2())), (empty, t2()) ))
    case _ => None
  }

  // official answer:
  // abstract one more layer deeper to zip 2 option2 in zipWithAll
  // but zipWithAll is not a exercise anyway
  def zipAll_1[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  // zip option tuple2
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // my answers:
  // use shortcut of foldRight directly
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).foldRight(true)( (o,v) => o match {
      case (Some(a), Some(b)) => if(a == b) v else false
      case (_, None) => true
      case (None, _) => false
    } )

  // offical answers:
  // reuse takeWhile to cut the stream to the length of prefix first
  // then match each value from left in turns
  def startsWith_1[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll (o => o._1 == o._2)

  // My answers with a little difference with official answers
  // In my consideration, on one hand, unfold will return at least emtpy stream
  // on the other hand, a nonempty stream always contains empty stream at tail
  // As conclusion, it is unnecessary to append empty stream after calling unfold.
  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some((s, t()))
    case _ => None
  }

  // my answer 1:
  // recursively call scanRight without duplicate evaluation
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h,t) => {
      val tail = t().scanRight(z)(f)
      cons(f(h(), tail.headOption.get), tail)
    }
  }

  def scanRight_o[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  // fix my answer 2 using foldRight with lazy variable to cache
  def scanRight_1[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight(Stream(z))((a,s) => {
      lazy val s1 = s
      cons(f(a,s1.headOption.get), s1)
    })
  }

  // by the way, one of biggest difference between the official and mine
  // is that they didn't use head Option to extract the first value of tail stream
  // but store extra first value of stream in the folded value as a tuple with the tail stream
  // fine, official answer is more natural while my answer still need to indirectly use foldRight
  // by using headOption to extract the head value
  // both way is okay, the official one is more autonomy


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
    unfold(a)(_ => Some(a,a))

  // official answer:
  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant_2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def from_1(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s+1)))

  def fibs() : Stream[Int] = {
    def go(x: Int, y: Int) : Stream[Int] =
      cons(x+y, go(y,x+y))
    cons(0,cons(1, go(0,1)))
  }

  // official way
  def fibs_2() : Stream[Int] = {
    def go(x: Int, y: Int) : Stream[Int] =
      cons(x, go(y,x+y))
    go(0,1)
  }

  def fibs_1() : Stream[Int] =
    unfold((0,1))(s => Some(s._1, (s._2, s._1 + s._2)))

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.
  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((x,y)) => cons(x, unfold(y)(f))
  }

  /*
  The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
   */
  // 2 unfold implementations from official
  // the meaning is learn to use Option in scala library to
  // fold value
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3).take(1).toList())
    println(Stream(1,2,3).take(2).toList())
    println(Stream(1,2,3).take(3).toList())
    println(Stream(1,2,3).take(5).toList())
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
    println(fibs_2().take(10).toList())
    println(from(11).take(10).toList())
    println(from_1(11).take(10).toList())
    println(constant_2(11).take_1(10).toList())
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

    println(Stream(1,2,3).scanRight(0)((a,b) => {
      println(a + ":" + b)
      a + b
    }).toList)
    println(Stream(1,2,3).scanRight_1(0)((a,b) => {
      println(a + ":" + b)
      a + b
    }).toList)
    println(Stream(1,2,3).scanRight_o(0)((a,b) => {
      println(a + ":" + b)
      a + b
    }).toList)

    println((Stream(),Stream()))
  }
}