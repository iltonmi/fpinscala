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

  // simplify using unit
  def map_1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))


  // simplify abs
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r2) = rng.nextInt
    (if(i > 0) i else -i + 1, r2)
  }

  // simplify division
  def double(rng: RNG): (Double, RNG) = {
    val (i, r2) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble + 1, r2)
  }

  def double_1(r: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i.doubleValue() / Int.MaxValue.doubleValue() + 1)(r)
  }

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

  // fix conner case
  // by the way, this can be rewritten to tail recursion
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List(), rng)
    val (l, r) = ints(count - 1)(rng)
    val (i, r2) = r.nextInt
    (i::l, r2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r => {
    val (a, rnga) = ra(r)
    val (b, rngb) = rb(rnga)
    (f(a,b), rngb)
  }

  def map2_1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => r => (f(a,b), r)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = r => {
    val h::t = fs
    val (l, r2) = sequence(t)(r)
    val (i, r3) = h(r2)
    (i::l, r3)
  }

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

case class State[S,+A](run: S => (A, S)) {
  def unit[A](a: A): State[S, A] = State(s => (a, s))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => State(s => (f(a,b), s))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a1,s1) = run(s)
    f(a1).run(s1)
  })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
