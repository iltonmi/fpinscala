package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // simplify using unit
  def map_1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // 6.1
  // simplify abs
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r2) = rng.nextInt
    (if(i > 0) i else -(i + 1), r2)
  }

  // 6.2
  // simplify division
  def double(rng: RNG): (Double, RNG) = {
    val (i, r2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r2)
  }

  def double_1(r: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i.doubleValue() / Int.MaxValue.doubleValue() + 1)(r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i,d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r2) = intDouble(rng)
    ((d,i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  // 6.4
  // fix conner case
  // by the way, this can be rewritten to tail recursion
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List(), rng)
    else {
      val (l, r) = ints(count - 1)(rng)
      val (i, r2) = r.nextInt
      (i::l, r2)
    }
  }

  // 6.5
  def doubleViaMap(rng: RNG) : (Double, RNG) =
    map(nonNegativeInt)(i => i.doubleValue() / (Int.MaxValue.doubleValue() + 1))(rng)

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r => {
    val (a, rnga) = ra(r)
    val (b, rngb) = rb(rnga)
    (f(a,b), rngb)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_,_))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // 6.7
  // expected result: RNG => (List[A], RNG)
  // (r1, (r2, (r3, ...)))
  // 0. x => (List(), x)
  // 1. x => ((a3), x)
  // 2. x => ((a2, a3)), x)
  // 3. x => 
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight((r : RNG) => (List[A](), r))((r, b) => map2(r, b)(_::_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = r => {
    val h::t = fs
    val (l, r2) = sequence(t)(r)
    val (i, r3) = h(r2)
    (i::l, r3)
  }

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = r => {
    val (a, rn) = f(r)
    g(a)(rn)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
    })

  // 6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  // rewrite using foldRight and map
  // the official answer implement using foldRight and map2
  def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] = r =>
    fs.foldRight((List[A](),r))((ra,p) => map(ra)(_::p._1)(p._2))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = r => {
    val (a, rn) = f(r)
    g(a)(rn)
  }

  def main(args: Array[String]): Unit = {
    val trans = State[Int, Int](s => (2, s))
    val (a, ns) = trans.run(1)
    val (a1, nns) = trans.run(ns)
  }
}

import State._

// 6.10
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => State(s => (f(a,b), s))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a1,s1) = run(s)
    f(a1).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  type Rand[A] = State[RNG, A]
  def update = (i: Input) => (m: Machine) => {
    (i, m) match {
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }
  }

  // given inputs as the event trigger state change, how will the machine state change
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map modify(update(_)))
    s <- get
  } yield (s.coins, s.candies)
}
