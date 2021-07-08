package com.streese.scala2.redbook.chapter06

import scala.annotation.tailrec

object snippets {

  sealed trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    case class SimpleRNG(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }
    }

    @tailrec
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, nextRng) = rng.nextInt
      if (i == Int.MinValue) nonNegativeInt(nextRng)
      else if (i < 0) i * -1 -> nextRng
      else            i      -> nextRng
    }

    // We need to be quite careful not to skew the generator.
    // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    // it suffices to increment the negative numbers by 1 and make them positive.
    // This maps Int.MinValue to Int.MaxValue and -1 to 0.
    def nonNegativeIntSmart(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i) -> r
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, nextRng) = rng.nextInt
      val dbl = i.toDouble / Int.MaxValue
      dbl.abs -> nextRng
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, fstRng) = rng.nextInt
      val (d, sndRng) = double(fstRng)
      (i -> d) -> sndRng
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), nextRng) = intDouble(rng)
      (d -> i) -> nextRng
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, fstRng)  = double(rng)
      val (d2, sndRng)  = double(fstRng)
      val (d3, thrdRng) = double(sndRng)
      (d1, d2, d3) -> thrdRng
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      List.fill(count)(0).foldLeft(List.empty: List[Int], rng){ case ((xs, rng), _) =>
        val (x, newRng) = rng.nextInt
        (x :: xs) -> newRng
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      f(a) -> rng2
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    def doubleElegant: Rand[Double] = map(_.nextInt)(i => (i.toDouble / Int.MaxValue).abs)

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      f(a, b) -> rng3
    }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

    def intDoubleElegant: Rand[(Int, Double)] = both(int, doubleElegant)

    def doubleIntElegant: Rand[(Double, Int)] = both(doubleElegant, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      fs.reverse.foldLeft(List.empty[A], rng){ case ((as, rng), ra) =>
        val (a, newRng) = ra(rng)
        (a :: as) -> newRng
      }
    }

    def intsViaSequene(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

    def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra){ a =>
        map(rb)(b => f(a, b))
      }
    }

  }

  case class State[S, +A](run: S => (A,S)) {

    def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      f(a) -> s2
    }
    )

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      f(a, b) -> s3
    }
    )


    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    }
    )

    def map2ViaFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap{ a =>
      sb.map(b => f(a, b))
    }

  }

  object State {

    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = State(
    s => {
      val (lOut, sOut) = l.foldLeft(List.empty[A], s){ case ((as, s), sa) =>
        val (a, newS) = sa.run(s)
        (a :: as) -> newS
      }
      lOut.reverse -> sOut // for some reason I have to do this here whereas below it's done on the input list
    }
    )

    def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def modifyUnsuggared[S](f: S => S): State[S, Unit] = get.flatMap(s => set(s).map(_ => ()))

  }

  object FiniteStateMachine {

    import State._

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {
      def receive(i: Input): Machine = (this, i) match {
        case (Machine(_    , ca, _ ), _   ) if ca < 1 => this
        case (Machine(true , ca, co), Coin)           => Machine(false, ca  , co+1)
        case (Machine(false, ca, co), Turn)           => Machine(true , ca-1, co  )
        case _                                        => this
      }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(
    in => {
      val out = inputs.foldLeft(in)((m, i) => m.receive(i))
      (out.coins, out.candies) -> out
    }
    )

    def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _))               => s
      case (Coin, Machine(false, _, _))        => s
      case (Turn, Machine(true, _, _))         => s
      case (Coin, Machine(true, candy, coin))  => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }

    // https://stackoverflow.com/questions/58587641/functional-programing-in-scala-exercise-6-11-how-does-this-for-comprehension-wo
    def simulateMachineIntended(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

  }

}
