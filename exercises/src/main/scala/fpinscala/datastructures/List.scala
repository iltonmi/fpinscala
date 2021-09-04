package fpinscala.datastructures

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

  val x = List(1,2,3,4,5) match {
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

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw Exception
      case Cons(_, rest) => rest
    }
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    Cons(h, tail(l))
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n > 0) {
      l match {
        case Nil => sys.error("")
        case Cons(h, t) => drop(t, n - 1)
      }
    }
    else l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw Exception
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l,0)((_,t) => 1 + t)
  }

  def length2[A](l : List[A]) =
    foldLeft(l, 0)((x,y) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // 3.11
  def sum3(l : List[Int]) =
    foldLeft(l, 0)(_+_)

  def product3(l : List[Double]) =
    foldLeft(l,1.0)(_*_)

  // 3.12
  def rev[A](l : List[A]) : List[A] =
    foldLeft(l, List[A]())((l, e) => Cons(e, l))
  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  // 3.16
  def allAddOne(l : List[Int]) : List[Int] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(1 + h, allAddOne((t)))
    }
  }

  // 3.17
  def doubleListToStrList(l : List[Double]) : List[String] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleListToStrList(t))
    }
  }

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if(f(h)) filter(t)(f)
        else Cons(h, filter(t)(f))
      }
    }
  }

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }
  }

  // 3.21
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => {
      if(f(x)) Nil
      else Cons(x, Nil)
    })
  }

  // 3.22
  def add(la : List[Int], lb : List[Int]) : List[Int] = {
    la match {
      case Nil => Nil
      case Cons(h,t) => {
        lb match {
          case Cons(h2,t2) => Cons(h + h2, add(t, t2))
        }
      }
    }
  }

  // 3.23
  def zipWith[A](la: List[A], lb: List[A])(f: (A, A) => A): List[A] = {
    la match {
      case Nil => Nil
      case Cons(h,t) => {
        lb match {
          case Cons(h2,t2) => Cons(f(h, h2), zipWith(t, t2)(f))
        }
      }
    }
  }
}
