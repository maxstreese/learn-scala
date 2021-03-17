package com.streese.scala2.redbook.chapter02

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class ExercisesSpec extends AnyFreeSpec with Matchers {

  "factorial should work" - {

    def test(f: Int => Int): Unit = {
      f(1) shouldBe 1
      f(2) shouldBe 2 * 1
      f(3) shouldBe 3 * 2 * 1
      f(4) shouldBe 4 * 3 * 2 * 1
    }

    "when non-tail-recursive" in {
      def factorial(n: Int): Int = if (n < 1) 1 else n * factorial(n - 1)
      test(factorial)
    }

    "when tail-recursive" in {
      def factorial(n: Int): Int = {
        @tailrec def go(n: Int, acc: Int): Int = if (n < 1) acc else go(n - 1, n * acc)
        go(n, 1)
      }
      test(factorial)
    }

  }

  "fibonacci should work" in {

    def fib(n: Int): Int = {
      @tailrec def go(n: Int, fst: Int, snd: Int): Int = n match {
        case 1 => fst
        case 2 => snd
        case n => go(n - 1, snd, fst + snd)
      }
      go(n, 0, 1)
    }

    fib(1) shouldBe 0
    fib(2) shouldBe 1
    fib(3) shouldBe 1
    fib(4) shouldBe 2
    fib(5) shouldBe 3
    fib(6) shouldBe 5

  }

  "isSorted should work" in {

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      def loop(n: Int): Boolean =
        if (n >= as.length) true
        else if (ordered(as(n-1), as(n))) loop(n+1)
        else false
      loop(1)
    }

    isSorted[Int](Array(1,2,3,4,5), _ < _) shouldBe true
    isSorted[Int](Array(1,2,7,4,5), _ < _) shouldBe false
    isSorted[Int](Array(1,2,3,4,5), _ > _) shouldBe false

  }

  "should compile" in {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)
  }

  "should compile" in {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  }

  "should compile" in {
    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  }

}
