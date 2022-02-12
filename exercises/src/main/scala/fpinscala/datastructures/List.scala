package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("list error")
      case Cons(_, rest) => rest
    }
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    Cons(h, tail(l))
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n > 0) {
      l match {
        case Nil => sys.error("list error")
        case Cons(_, t) => drop(t, n - 1)
      }
    }
    else l
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("list error")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l,0)((_,t) => 1 + t)
  }

  def length2[A](l : List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // 3.11
  def sum3(l : List[Int]): Int =
    foldLeft(l, 0)(_+_)

  def product3(l : List[Double]): Double =
    foldLeft(l,1.0)(_*_)

  // 3.12
  def rev[A](l : List[A]) : List[A] =
    foldLeft(l, List[A]())((l, e) => Cons(e, l))

  def revViaFoldLeft[A](l : List[A]) : List[A] = {
    // (1, (2, (3, ...))) -> (3, (2, (1, ...)))
    // x -> 1 cons x -> 2 cons (1 cons x) -> 3 cons (2 cons (1 cons x))
    foldLeft(l, List[A]())((l, e) => Cons(e, l))
  }

  def revViaFoldRight[A](l : List[A]) : List[A] =
    // (1, (2, (3, ...))) -> (3, (2, (1, ...)))
    // x -> 3 cons x ->  2 cons (3 cons x) -> 1 cons (2 cons (3 cons x))
    foldRight(l, (x : List[A]) => x)((a, g) => x => g(Cons(a, x)))(Nil:List[A])

  // 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    // reverse using foldRight, then foldRight on reversed list
    foldRight(foldRight(l, Nil:List[A])(Cons(_, _)), z)((a, b) => f(b, a))
  }

  def foldLeftViaFoldRight_1[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    // f(((b, 1), 2), 3)
    // On one hand, function always evaluated outwards
    // On the other hand, foldRight will fold the rightmost element first
    // As a result, when the result of foldRight is of function type, the first folded element which is also the rightmost
    // element will come into the build up process first and turns out to be the parameter of the outmost function
    // which will be evaluated at last.
    // for foldLeft, given: (1,2,3),0,f
    // expected result: (((0 f 1) f 2) f 3)
    // recursive process:
    // 0. init: x
    // 1. apply 3 -> x f 3 => x f 3 -> x f 3
    // 2. apply 2 -> (x f 2) f 3
    // 3. apply 1 -> ((x f 1) f 2) f 3
    // recursively replace x with (x f curVal)
    foldRight(l, (b : B) => b)((e, g) => b => g(f(b, e)))(z)
  }


  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    // expected result: (1 f (2 f (3 f x)))
    // recursively replace x with (curVal f x)
    // x -> 1 f x -> 1 f (2 f x) -> 1 f (2 f (3 f x))
    // that is x -> f(1, x) -> f(1, f(2, x)) -> f(1, f(2, f(3, x)))
    foldLeft(as, (x : B) => x)((g, e) => x => g(f(e, x)))(z)
  }

  // 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = {
    // (1, 2, 3) (4, 5, 6)
    // expected result: (1, (2, (3,(4, 5, 6))))
    // 1 cons (2 cons (3 cons (...)))
    // x -> 1 cons x -> 1 cons (2 cons x) -> 1 cons (2 cons (3 cons x))
    // recursively replace x with (curVal cons x)
    foldLeft(l, (x : List[A]) => x)((g, a) => x => g(Cons(a, x)))(r)
  }

  // 3.15
  def concat[A](ll : List[List[A]]) : List[A] = {
    foldRight(ll, Nil:List[A])(appendViaFoldRight)
  }

  // 3.16
  def allAddOne(l : List[Int]) : List[Int] =
    foldRight(l, Nil:List[Int])((x, l) => Cons(x+1,l))

  // 3.17
  def doubleListToStrList(l : List[Double]) : List[String] =
    foldRight(l, Nil:List[String])((x,l) => Cons(x.toString, l))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((x,l) => Cons(f(x), l))

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((x, l) => if(f(x)) Cons(x, l) else l)

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.21
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) List(x) else Nil)

  // 3.22
  def add(la : List[Int], lb : List[Int]) : List[Int] = (la, lb) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, add(t1, t2))
  }

  // 3.23
  def zipWith[A](la: List[A], lb: List[A])(f: (A, A) => A): List[A] = (la, lb) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // 3.24
  @tailrec
  def startsWith[A](l : List[A], pre : List[A]): Boolean = (l, pre) match {
    case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => startsWith(t1,t2)
    case (_, Nil) => true
    case (Nil, _) => false
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case Cons(_, t) => startsWith(l, sub) || hasSubsequence(t, sub)
  }
}
