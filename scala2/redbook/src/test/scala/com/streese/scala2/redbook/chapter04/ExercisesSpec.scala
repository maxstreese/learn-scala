package com.streese.scala2.redbook.chapter04

import com.streese.scala2.redbook.chapter04.snippets._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ExercisesSpec extends AnyFreeSpec with Matchers {

  "FunOption exercises" - {

    val some: FunOption[Int] = Some(1)
    val none: FunOption[Int] = None

    "4.1 map" in {
      some.map(_ + 1) shouldBe Some(2)
      none.map(_ + 1) shouldBe None
    }

    "4.1 flatMap" in {
      some.flatMap(x => Some(x + 1)) shouldBe Some(2)
      some.flatMap(_ => None)        shouldBe None
      none.flatMap(x => Some(x + 1)) shouldBe None
    }

    "4.1 getOrElse" in {
      some.getOrElse(2) shouldBe 1
      none.getOrElse(2) shouldBe 2
    }

    "4.1 orElse" in {
      some.orElse(Some(2)) shouldBe Some(1)
      none.orElse(Some(2)) shouldBe Some(2)
    }

    "4.1 filter" in {
      some.filter(_ == 1) shouldBe Some(1)
      some.filter(_ == 2) shouldBe None
      none.filter(_ == 1) shouldBe None
    }

    "4.2 variance" in {
      val xs = Seq(3.0,21.0,98.0,203.0,17.0,9.0)
      mean(xs)            shouldBe Some(58.5)
      variance(xs)        shouldBe Some(5183.25)
      variance(Seq.empty) shouldBe None
    }

    "4.3 map2" in {
      val f = (a: Int, b: Int) => a + b
      map2(some, some)(f) shouldBe Some(2)
      map2(none, some)(f) shouldBe None
      map2(some, none)(f) shouldBe None
    }

    "4.4 sequence" in {
      sequence(List(some))       shouldBe Some(List(1))
      sequence(List(some, some)) shouldBe Some(List(1, 1))
      sequence(List(none))       shouldBe None
      sequence(List(none, none)) shouldBe None
      sequence(List(some, none)) shouldBe None
      sequence(List(none, some)) shouldBe None
    }

    "4.5 traverse" in {
      def f(x: Int): FunOption[Int] = if (x == 1) Some(1) else None

      traverse[Int, Int](List(1))(f)    shouldBe Some(List(1))
      traverse[Int, Int](List(1, 1))(f) shouldBe Some(List(1, 1))
      traverse[Int, Int](List(2))(f)    shouldBe None
      traverse[Int, Int](List(2, 2))(f) shouldBe None
      traverse[Int, Int](List(1, 2))(f) shouldBe None
      traverse[Int, Int](List(2, 1))(f) shouldBe None

      sequenceViaTraverse(List(some))       shouldBe Some(List(1))
      sequenceViaTraverse(List(some, some)) shouldBe Some(List(1, 1))
      sequenceViaTraverse(List(none))       shouldBe None
      sequenceViaTraverse(List(none, none)) shouldBe None
      sequenceViaTraverse(List(some, none)) shouldBe None
      sequenceViaTraverse(List(none, some)) shouldBe None

    }

  }

  "FunEither exercises" - {
    val right: FunEither[Int, Int] = Right(1)
    val left:  FunEither[Int, Int] = Left(1)

    "4.6" - {

      "map" in {
        right.map(_ + 1) shouldBe Right(2)
        left.map(_ + 1)  shouldBe Left(1)
      }

      "flatMap" in {
        right.flatMap(x => Right(x + 1)) shouldBe Right(2)
        right.flatMap(_ => left)         shouldBe left
        left.flatMap(x => Right(x + 1))  shouldBe left
      }

      "orElse" in {
        right.orElse(Right(2)) shouldBe right
        right.orElse(left)     shouldBe right
        left.orElse(right)     shouldBe right
        left.orElse(left)      shouldBe left
      }

      "map2" in {
        val f = (a: Int, b: Int) => a + b
        right.map2(right)(f) shouldBe Right(2)
        right.map2(left)(f)  shouldBe left
        left.map2(right)(f)  shouldBe left
        left.map2(left)(f)   shouldBe left
      }

    }

  }

}
