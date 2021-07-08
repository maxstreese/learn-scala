package com.streese.scala2.redbook.chapter06

import com.streese.scala2.redbook.chapter06.snippets._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.annotation.tailrec

class ExercisesSpec extends AnyFreeSpec with ScalaCheckPropertyChecks with Matchers {


  "Random Number Generator" - {

    import com.streese.scala2.redbook.chapter06.snippets.RNG._

    val fixedSeedSimpleRng = SimpleRNG(1)

    "6.1 nonNegativeInt" in {
      forAll { s: Long =>
        val rng = SimpleRNG(s)
        nonNegativeInt(rng)._1 should be >= 0
      }
    }

    "6.2 double" in {
      forAll { s: Long =>
        val rng = SimpleRNG(s)
        double(rng)._1 should (be >= 0.0 and be < 1.0)
      }
    }

    "6.3" - {
      "intDouble" in {}
      "doubleInt" in {}
      "double3" in {}
    }

    "6.4 ints" in {
      ints(-1)(fixedSeedSimpleRng)._1.size shouldBe 0
      ints(0)(fixedSeedSimpleRng)._1.size  shouldBe 0
      ints(1)(fixedSeedSimpleRng)._1.size  shouldBe 1
      ints(2)(fixedSeedSimpleRng)._1.size  shouldBe 2
      // Test that different numbers are generated and not always the same. This is not an ideal test though as there
      // is a probability greater than zero that the exact same integer is generated multiple times in a row. Hence why
      // the fixed seed here
      ints(10)(fixedSeedSimpleRng)._1.toSet.size  shouldBe 10
    }

    "6.5 doubleElegant" in {
      forAll { s: Long =>
        val rng = SimpleRNG(s)
        double(rng)._1 should (be >= 0.0 and be < 1.0)
      }
    }

    "6.7" - {
      "intsViaSequence" in {
        intsViaSequene(-1)(fixedSeedSimpleRng)._1.size shouldBe 0
        intsViaSequene(0)(fixedSeedSimpleRng)._1.size  shouldBe 0
        intsViaSequene(1)(fixedSeedSimpleRng)._1.size  shouldBe 1
        intsViaSequene(2)(fixedSeedSimpleRng)._1.size  shouldBe 2
        intsViaSequene(10)(fixedSeedSimpleRng)._1.toSet.size  shouldBe 10
      }
    }

    "6.8 nonNegativeLessThan" in {
      forAll { s: Long =>
        val rng = SimpleRNG(s)
        nonNegativeLessThan(25)(rng)._1 should (be >= 0 and be < 25)
      }
    }

  }

  "6.10 State" - {

    import com.streese.scala2.redbook.chapter06.snippets.State._

    "test my understanding" in {
      val (_, i) = get[Int].run(1)
      i shouldBe 1
      val (_, j) = set[Int](2).run(1)
      j shouldBe 2
      val (_, k) = modify[Int](_ + 1).run(1)
      k shouldBe 2
    }

    "sequence" in {
      def a: State[Int, Int] = unit[Int, Int](1)
      def b: State[Int, Int] = unit[Int, Int](2)
      def c: State[Int, Int] = unit[Int, Int](3)
      sequence(List(a,b,c)).run(0)            shouldBe (List(1,2,3), 0)
      sequenceViaFoldLeft(List(a,b,c)).run(0) shouldBe (List(1,2,3), 0)
    }

  }

  "6.1 Finite State Machine" - {

    import com.streese.scala2.redbook.chapter06.snippets.State._
    import com.streese.scala2.redbook.chapter06.snippets.FiniteStateMachine._

    "should pass the example given in the book for" - {

      val machine = Machine(true, 5, 10)
      val inputs  = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

      "my implementation" in {
        val ((coins, candies), _) = simulateMachine(inputs).run(machine)
        coins   shouldBe 14
        candies shouldBe 1
      }

      "the authors implementation" in {
        val ((coins, candies), _) = simulateMachineIntended(inputs).run(machine)
        coins   shouldBe 14
        candies shouldBe 1
      }

    }

    // just here for me to understand stuff
    "requires function composition" in {
      def f(s: String): String = s"f($s)"
      def g(s: String): String = s"g($s)"
      val gComposeF = g _ compose f
      val gAndThenF = g _ andThen f
      gComposeF("s") shouldBe "g(f(s))"
      gAndThenF("s") shouldBe "f(g(s))"
    }

  }

}
