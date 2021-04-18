package com.streese.scala2.redbook.chapter05

import com.streese.scala2.redbook.chapter05.snippets._
import com.streese.scala2.redbook.chapter05.snippets.Stream._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ExercisesSpec extends AnyFreeSpec with Matchers {

  val withSideEffects = false

  def s0: Stream[Int] = Stream.empty
  def s1: Stream[Int] =
    if (withSideEffects) cons({println("1"); 1}, Stream.empty)
    else cons(1, Stream.empty)
  def s2: Stream[Int] =
    if (withSideEffects) cons({println("1"); 1}, cons({println("2"); 2}, Stream.empty))
    else cons(1, cons(2, Stream.empty))
  def s3: Stream[Int] =
    if (withSideEffects) cons({println("1"); 1}, cons({println("2"); 2}, cons({println("3"); 3}, Stream.empty)))
    else cons(1, cons(2, cons(3, Stream.empty)))

  "5.1 toList" in {
    s0.toList shouldBe List.empty
    s1.toList shouldBe List(1)
    s2.toList shouldBe List(1, 2)
    s3.toList shouldBe List(1, 2, 3)
  }

  "5.2" - {

    "take" in {
      s0.take(0).toList shouldBe List.empty
      s0.take(1).toList shouldBe List.empty
      s3.take(0).toList shouldBe List.empty
      s3.take(1).toList shouldBe List(1)
      s3.take(2).toList shouldBe List(1, 2)
      s3.take(3).toList shouldBe List(1, 2, 3)
      s3.take(4).toList shouldBe List(1, 2, 3)
    }

    "drop" in {
      s0.drop(0).toList shouldBe List.empty
      s0.drop(1).toList shouldBe List.empty
      s3.drop(0).toList shouldBe List(1, 2, 3)
      s3.drop(1).toList shouldBe List(2, 3)
      s3.drop(2).toList shouldBe List(3)
      s3.drop(3).toList shouldBe List.empty
      s3.drop(4).toList shouldBe List.empty
    }

  }

  "5.3 takeWhile" in {
      s0.takeWhile(_ < 0).toList shouldBe List.empty
      s3.takeWhile(_ < 0).toList shouldBe List.empty
      s3.takeWhile(_ < 2).toList shouldBe List(1)
      s3.takeWhile(_ < 3).toList shouldBe List(1, 2)
      s3.takeWhile(_ < 4).toList shouldBe List(1, 2, 3)
      s3.takeWhile(_ < 5).toList shouldBe List(1, 2, 3)
  }

  "5.4 forAll" in {
    val f = (i: Int) => i == 1
    s0.forAll(f) shouldBe true
    s1.forAll(f) shouldBe true
    s2.forAll(f) shouldBe false
    s3.forAll(f) shouldBe false
  }

  "5.5 takeWhileViaFoldRight" in {
      s0.takeWhileViaFoldRight(_ < 0).toList shouldBe List.empty
      s3.takeWhileViaFoldRight(_ < 0).toList shouldBe List.empty
      s3.takeWhileViaFoldRight(_ < 2).toList shouldBe List(1)
      s3.takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)
      s3.takeWhileViaFoldRight(_ < 4).toList shouldBe List(1, 2, 3)
      s3.takeWhileViaFoldRight(_ < 5).toList shouldBe List(1, 2, 3)
  }

  "5.6 headOptionViaFoldRight" in {
    s0.headOption shouldBe None
    s1.headOption shouldBe Some(1)
    s2.headOption shouldBe Some(1)
  }

  "5.7" - {

    "map" in {
      val f = (i: Int) => i + 1
      s0.map(f).toList shouldBe List.empty
      s1.map(f).toList shouldBe List(2)
      s2.map(f).toList shouldBe List(2, 3)
      s3.map(f).toList shouldBe List(2, 3, 4)
    }

    "filter" in {
      val f = (i: Int) => i % 2 == 0
      s0.filter(f).toList shouldBe List.empty
      s1.filter(f).toList shouldBe List.empty
      s2.filter(f).toList shouldBe List(2)
      s3.filter(f).toList shouldBe List(2)
    }

    "append" in {
      s0.append(s0).toList shouldBe List.empty
      s0.append(s1).toList shouldBe List(1)
      s1.append(s2).toList shouldBe List(1, 1, 2)
      s2.append(s1).toList shouldBe List(1, 2, 1)
    }

    "flatMap" in {
      val fEmpty = (i: Int) => Stream.empty[Int]
      val fOne   = (i: Int) => Stream(i)
      val fTwo   = (i: Int) => Stream(i, i + 1)
      s0.flatMap(fEmpty).toList shouldBe List.empty
      s0.flatMap(fOne).toList   shouldBe List.empty
      s0.flatMap(fTwo).toList   shouldBe List.empty
      s1.flatMap(fEmpty).toList shouldBe List.empty
      s1.flatMap(fOne).toList   shouldBe List(1)
      s1.flatMap(fTwo).toList   shouldBe List(1, 2)
      s2.flatMap(fEmpty).toList shouldBe List.empty
      s2.flatMap(fOne).toList   shouldBe List(1, 2)
      s2.flatMap(fTwo).toList   shouldBe List(1, 2, 2, 3)
    }

  }

  "5.8 constant" in {
    Stream.constant(1).take(3).toList shouldBe List(1, 1, 1)
  }

  "5.9 from" in {
    Stream.from(0).take(3).toList shouldBe List(0, 1, 2)
    Stream.from(1).take(3).toList shouldBe List(1, 2, 3)
  }

  "5.10 fibs" in {
    Stream.fibs.take(1).toList  shouldBe List(0)
    Stream.fibs.take(2).toList  shouldBe List(0,1)
    Stream.fibs.take(3).toList  shouldBe List(0,1,1)
    Stream.fibs.take(4).toList  shouldBe List(0,1,1,2)
    Stream.fibs.take(5).toList  shouldBe List(0,1,1,2,3)
    Stream.fibs.take(6).toList  shouldBe List(0,1,1,2,3,5)
    Stream.fibs.take(7).toList  shouldBe List(0,1,1,2,3,5,8)
    Stream.fibs.take(8).toList  shouldBe List(0,1,1,2,3,5,8,13)
    Stream.fibs.take(9).toList  shouldBe List(0,1,1,2,3,5,8,13,21)
    Stream.fibs.take(10).toList shouldBe List(0,1,1,2,3,5,8,13,21,34)
  }

  "5.11 unfold" in {
  }

  "5.12 viaUnfold" - {

    "ones" in {
      Stream.onesViaUnfold.take(1).toList shouldBe List(1)
      Stream.onesViaUnfold.take(2).toList shouldBe List(1,1)
      Stream.onesViaUnfold.take(3).toList shouldBe List(1,1,1)
    }

    "constant" in {
      Stream.constantViaUnfold(1).take(3).toList shouldBe List(1, 1, 1)
    }

    "from" in {
      Stream.fromViaUnfold(0).take(3).toList shouldBe List(0, 1, 2)
      Stream.fromViaUnfold(1).take(3).toList shouldBe List(1, 2, 3)
    }

    "fibs" in {
      Stream.fibsViaUnfold.take(1).toList  shouldBe List(0)
      Stream.fibsViaUnfold.take(2).toList  shouldBe List(0,1)
      Stream.fibsViaUnfold.take(3).toList  shouldBe List(0,1,1)
      Stream.fibsViaUnfold.take(4).toList  shouldBe List(0,1,1,2)
      Stream.fibsViaUnfold.take(5).toList  shouldBe List(0,1,1,2,3)
      Stream.fibsViaUnfold.take(6).toList  shouldBe List(0,1,1,2,3,5)
      Stream.fibsViaUnfold.take(7).toList  shouldBe List(0,1,1,2,3,5,8)
      Stream.fibsViaUnfold.take(8).toList  shouldBe List(0,1,1,2,3,5,8,13)
      Stream.fibsViaUnfold.take(9).toList  shouldBe List(0,1,1,2,3,5,8,13,21)
      Stream.fibsViaUnfold.take(10).toList shouldBe List(0,1,1,2,3,5,8,13,21,34)
    }

  }

  "5.13 viaUnfold" - {

    "map" in {
      val f = (i: Int) => i + 1
      s0.mapViaUnfold(f).toList shouldBe List.empty
      s1.mapViaUnfold(f).toList shouldBe List(2)
      s2.mapViaUnfold(f).toList shouldBe List(2, 3)
      s3.mapViaUnfold(f).toList shouldBe List(2, 3, 4)
    }

    "take" in {
      s0.takeViaUnfold(0).toList shouldBe List.empty
      s0.takeViaUnfold(1).toList shouldBe List.empty
      s3.takeViaUnfold(0).toList shouldBe List.empty
      s3.takeViaUnfold(1).toList shouldBe List(1)
      s3.takeViaUnfold(2).toList shouldBe List(1, 2)
      s3.takeViaUnfold(3).toList shouldBe List(1, 2, 3)
      s3.takeViaUnfold(4).toList shouldBe List(1, 2, 3)
    }

    "takeWhile" in {
      s0.takeWhileViaUnfold(_ < 0).toList shouldBe List.empty
      s3.takeWhileViaUnfold(_ < 0).toList shouldBe List.empty
      s3.takeWhileViaUnfold(_ < 2).toList shouldBe List(1)
      s3.takeWhileViaUnfold(_ < 3).toList shouldBe List(1, 2)
      s3.takeWhileViaUnfold(_ < 4).toList shouldBe List(1, 2, 3)
      s3.takeWhileViaUnfold(_ < 5).toList shouldBe List(1, 2, 3)
    }

    "zipWith" in {
      s0.zipWithViaUnfold(s0)(_ + _).toList shouldBe List.empty
      s1.zipWithViaUnfold(s0)(_ + _).toList shouldBe List.empty
      s0.zipWithViaUnfold(s1)(_ + _).toList shouldBe List.empty
      s2.zipWithViaUnfold(s3)(_ + _).toList shouldBe List(2,4)
      s3.zipWithViaUnfold(s2)(_ + _).toList shouldBe List(2,4)
      s3.zipWithViaUnfold(s3)(_ + _).toList shouldBe List(2,4,6)
    }

    "zipAll" in {
      s0.zipAllViaUnfold(s0).toList shouldBe List.empty
      s1.zipAllViaUnfold(s0).toList shouldBe List((Some(1), None))
      s3.zipAllViaUnfold(s2).toList shouldBe List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
      s2.zipAllViaUnfold(s3).toList shouldBe List((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3)))
    }

  }

  "5.14 startsWith" in {
    s0.startsWith(s0)   shouldBe true
    s1.startsWith(s0)   shouldBe true
    s0.startsWith(s1)   shouldBe false
    s1.startsWith(s1)   shouldBe true
    s2.startsWith(s3)   shouldBe false
    s3.startsWith(s2)   shouldBe true
    s3.startsWith(s3)   shouldBe true
    ones.startsWith(s0) shouldBe true
    ones.startsWith(s1) shouldBe true
    ones.startsWith(s2) shouldBe false
  }

  "5.15 tails" in {
    s0.tails.toList.map(_.toList) shouldBe List.empty
    s1.tails.toList.map(_.toList) shouldBe List(List(1))
    s2.tails.toList.map(_.toList) shouldBe List(List(1,2), List(2))
    s3.tails.toList.map(_.toList) shouldBe List(List(1,2,3), List(2,3), List(3))
    ones.tails.take(3).toList.map(_.take(1).toList).flatten shouldBe List(1,1,1)
  }

  "5.16 scanRight" in {
    s0.scanRight(0)(_ + _).toList shouldBe List(0)
    s1.scanRight(0)(_ + _).toList shouldBe List(1,0)
    s2.scanRight(0)(_ + _).toList shouldBe List(3,2,0)
    s3.scanRight(0)(_ + _).toList shouldBe List(6,5,3,0)
  }

  "5.16 scanRight lazy" in {
    from(0).scanRight(0)((a, acc) => if (a < 10) a + acc else a).take(10).toList
  }

  "5.16 scanRight why not lazy?" in {
    from(0).scanRightWhyNotLazy(0)((a, acc) => if (a < 10) a + acc else a).take(10).toList
  }

}
