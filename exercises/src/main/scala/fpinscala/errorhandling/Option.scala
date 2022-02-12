package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  // map first then get the option
  def flatMap[B](f: A => Option[B]): Option[B] = {
    // f(getOrElse(None)) is wrong, cuz parameter of f should not be of type None but only A
    map(f).getOrElse(None)
  }

  // map to nested options, then getOrElse
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    // expected result Option[A] or Option[B]
    // at first glance, we could use this.getOrElse(ob)
    // but it's wrong, because it return A or Option[B]
    // so we can wrap this into Option again, so that Option[Option[A]].getOrElse(ob) return Option[A] or Option[B]
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    // flatMap accept a map function to Option directly
    flatMap(a => if (f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  // map and flatmap can be used to extract value
  // select between 2 map base on the result type for convenience
  def variance(xs: Seq[Double]): Option[Double] = {
    // cuz result type is option, so use flatMap other than map
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  // 4.4
  // List maybe Nil.... okay, a Scala syntax problem...
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t).map(hh :: _))
  }

  // chain each element on the left to the list on the right
  // using fold right
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    // (o1, (o2, (o3, x))) -> o of (1, (2, (3, x)))
    // x -> (o3, x)
    a.foldRight[Option[List[A]]](Some(Nil))((oe, ol) => map2(oe, ol)(_ :: _))
  }

  // 思路和sequence是一样的
  // 都是用头元素和尾列表相连
  // 具体方式：递归或foldRight
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((e, ol) => map2(f(e), ol)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverseViaSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)
}