package com.streese.scala2.redbook.chapter03

import java.time.{Duration, Instant}

import com.streese.scala2.redbook.chapter03.snippets._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ExercisesSpec extends AnyFreeSpec with Matchers {

  "FunList exercises" - {

    import com.streese.scala2.redbook.chapter03.snippets.FunList._

    "3.1 determine the result of a match" in {
      val x = FunList(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }

      x shouldBe 3

    }

    "3.4 drop" in {
      val l = FunList(1,2,3,4,5,6)
      drop(l, 0) shouldBe FunList(1,2,3,4,5,6)
      drop(l, 1) shouldBe FunList(2,3,4,5,6)
      drop(l, 2) shouldBe FunList(3,4,5,6)
      drop(l, 3) shouldBe FunList(4,5,6)
      drop(l, 4) shouldBe FunList(5,6)
      drop(l, 5) shouldBe FunList(6)
      drop(l, 6) shouldBe Nil
      drop(l, 7) shouldBe Nil
    }

    "3.5 dropWhile" in {
      val l = FunList(1,2,3,4,5,6)
      dropWhile[Int](l, _ < 1) shouldBe FunList(1,2,3,4,5,6)
      dropWhile[Int](l, _ < 3) shouldBe FunList(3,4,5,6)
      dropWhile[Int](l, _ < 7) shouldBe Nil
    }

    "3.6 init" in {
      val l = FunList(1,2,3,4,5,6)
      init(l) shouldBe FunList(1,2,3,4,5)
    }

    "3.8 foldRight as a constructor?" in {
      val l = FunList(1,2,3)
      foldRight(l, Nil: FunList[Int])(Cons(_, _)) shouldBe l
    }

    "3.9 lengthViaFoldRight" in {
      lengthViaFoldRight(Nil) shouldBe 0
      lengthViaFoldRight(FunList(2)) shouldBe 1
      lengthViaFoldRight(FunList(2, 3, 4)) shouldBe 3
    }

    "3.10 foldLeft" in {
      val l1: FunList[Int] = Nil
      val l2               = FunList(1,2,3,4,5,6)
      foldLeft(l1, 0)(_ + _) shouldBe foldRight(l1, 0)(_ + _)
      foldLeft(l2, 0)(_ + _) shouldBe foldRight(l2, 0)(_ + _)
      foldLeft(l1, 1)(_ * _) shouldBe foldRight(l1, 1)(_ * _)
      foldLeft(l2, 1)(_ * _) shouldBe foldRight(l2, 1)(_ * _)
    }

    "3.12 reverseViaFoldLeft" in {
      reverseViaFoldLeft(FunList(1,2,3)) shouldBe FunList(3,2,1)
    }

    "3.13 folds in terms of each other" in {

      val l        = FunList(1,2,3)
      val resLeft  = ((0 - 1) - 2) - 3
      val resRight = 1 - (2 - (3 - 0))

      foldLeft(l, 0)(_ - _)  shouldBe resLeft
      foldRight(l, 0)(_ - _) shouldBe resRight

      foldLeftViaFoldRight(l, 0)(_ - _) shouldBe resLeft
      foldRightViaFoldLeft(l, 0)(_ - _) shouldBe resRight

    }

    "3.14 appendViaFoldLeft" in {
      appendViaFoldRight(FunList(1,2,3), FunList(4,5,6)) shouldBe FunList(1,2,3,4,5,6)
    }

    "3.15 concatViaFoldLeft" in {
      concatViaFoldLeft(FunList(FunList(1,2,3), FunList(4,5,6))) shouldBe FunList(1,2,3,4,5,6)
    }

    "3.15 concatViaFoldRight" in {
      concatViaFoldRight(FunList(FunList(1,2,3), FunList(4,5,6))) shouldBe FunList(1,2,3,4,5,6)
    }

    "3.16 addOneViaFoldLeft" in {
      addOneViaFoldRight(FunList(1,2,3)) shouldBe FunList(2,3,4)
    }

    "3.17 doublesToStringsViaFoldRight" in {
      doublesToStringsViaFoldRight(FunList(1.0,2.0,3.0)) shouldBe FunList("1.0","2.0","3.0")
    }

    "3.18 mapViaFoldRight" in {
      mapViaFoldRight(FunList(1,2,3))(_ + 1) shouldBe FunList(2,3,4)
      mapViaFoldRight(FunList(1.0,2.0,3.0))(_.toString) shouldBe FunList("1.0", "2.0", "3.0")
    }

    "3.19 filterViaFoldRight" in {
      filterViaFoldRight(FunList(1,2,3,4))(_ % 2 == 0) shouldBe FunList(2,4)
    }

    "3.20 flatMapViaFoldRight" in {
      flatMapViaFoldRight(FunList(1,2,3))(i => FunList(i, i)) shouldBe FunList(1,1,2,2,3,3)
    }

    "3.21 filterViaFlatMap" in {
      filterViaFlatMap(FunList(1,2,3,4))(_ % 2 == 0) shouldBe FunList(2,4)
    }

    "3.22 addElements" in {
      addElements(FunList(1,2,3), FunList(4,5,6)) shouldBe FunList(5,7,9)
    }

    "3.22 addElementsViaFoldLeft" in {
      addElementsViaFoldLeft(FunList(1,2,3), FunList(4,5,6)) shouldBe FunList(5,7,9)
    }

    "3.23 zipWithViaFoldLeft" in {
      zipWithViaFoldLeft(FunList(1,2,3), FunList(4,5,6))(_ + _) shouldBe FunList(5,7,9)
    }

    "3.24 hasSubsequence" in {
      hasSubsequence(FunList(1,2,3), Nil)            shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(1))     shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(1,2))   shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(1,2,3)) shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(2,3))   shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(3))     shouldBe true
      hasSubsequence(FunList(1,2,3), FunList(3,4))   shouldBe false
      hasSubsequence(Nil, FunList(1))                shouldBe false
    }

  }


  "Tree exercises" - {

    import com.streese.scala2.redbook.chapter03.snippets.Tree._

    val t1 = Leaf(1)
    val t2 = Branch(
      Leaf(1),
      Leaf(2)
    )
    val t3 = Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf(3),
        Leaf(4)
      )
    )

    "3.25 size" in {
      Tree.size(t1) shouldBe 1
      Tree.size(t2) shouldBe 3
      Tree.size(t3) shouldBe 7
    }

    "3.26 maximum" in {
      maximum(t1) shouldBe 1
      maximum(t2) shouldBe 2
      maximum(t3) shouldBe 4
    }

    "3.27 depth" in {
      depth(t1) shouldBe 0
      depth(t2) shouldBe 1
      depth(t3) shouldBe 2
    }

    "3.28 map" in {
      map(t2)(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
    }

    "3.29 sizeViaFold" in {
      sizeViaFold(t1) shouldBe 1
      sizeViaFold(t2) shouldBe 3
      sizeViaFold(t3) shouldBe 7
    }

    "3.29 maximumViaFold" in {
      maximumViaFold(t1) shouldBe 1
      maximumViaFold(t2) shouldBe 2
      maximumViaFold(t3) shouldBe 4
    }

    "3.29 depthViaFold" in {
      depthViaFold(t1) shouldBe 0
      depthViaFold(t2) shouldBe 1
      depthViaFold(t3) shouldBe 2
    }

    "3.29 mapViaFold" in {
      mapViaFold(t2)(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
    }

  }

}
