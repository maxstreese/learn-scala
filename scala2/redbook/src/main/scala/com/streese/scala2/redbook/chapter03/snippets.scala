package com.streese.scala2.redbook.chapter03

import scala.annotation.tailrec

object snippets {

  sealed trait FunList[+A]
  case object  Nil                                 extends FunList[Nothing]
  case class   Cons[+A](head: A, tail: FunList[A]) extends FunList[A]

  object FunList {

    def apply[A](as: A*): FunList[A] =
      if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    def sum(ints: FunList[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: FunList[Double]): Double = ds match {
      case Nil         => 1
      case Cons(x, xs) => x * product(xs)
    }

    def tail[A](l: FunList[A]) = l match {
      case Nil        => Nil
      case Cons(h, t) => t
    }

    def setHead[A](l: FunList[A], elem: A) = l match {
      case Nil        => Cons(elem, Nil)
      case Cons(_, t) => Cons(elem, t)
    }

    @tailrec
    def drop[A](l: FunList[A], n: Int): FunList[A] = {
      if (n < 1) l
      else l match {
        case Nil        => Nil
        case Cons(h, t) => drop(t, n-1)
      }
    }

    @tailrec
    def dropWhile[A](l: FunList[A], f: A => Boolean): FunList[A] = l match {
      case Nil        => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }

    def reverse[A](l: FunList[A]): FunList[A] = {
      @tailrec
      def go(l: FunList[A], acc: FunList[A]): FunList[A] = l match {
        case Nil => acc
        case Cons(h, t) => go(t, Cons(h, acc))
      }
      go(l, Nil)
    }

    def init[A](l: FunList[A]): FunList[A] = {
      @tailrec
      def go(l: FunList[A], acc: FunList[A]): FunList[A] = l match {
        case Nil                 => acc
        case Cons(head, Nil)     => acc
        case Cons(x, Cons(y, t)) => go(Cons(y, t), Cons(x, acc))
      }
      reverse(go(l, Nil))
    }

    def foldRight[A, B](as: FunList[A], z: B)(f: (A, B) => B): B = as match {
      case Nil        => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    def lengthViaFoldRight[A](as: FunList[A]): Int =
      foldRight(as, 0)((_, acc) => acc + 1)

    @tailrec
    def foldLeft[A, B](as: FunList[A], z: B)(f: (B, A) => B): B = as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def sumViaFoldLeft(as: FunList[Int]): Int =
      foldLeft(as, 0)(_ + _)

    def productViaFoldLeft(as: FunList[Double]): Double =
      foldLeft(as, 1.0)(_ * _)

    def lengthViaFoldLeft[A](as: FunList[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverseViaFoldLeft[A](as: FunList[A]): FunList[A] =
      foldLeft(as, Nil: FunList[A])((acc, a) => Cons(a, acc))

    def foldRightViaFoldLeft[A, B](as: FunList[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    def foldLeftViaFoldRight[A, B](as: FunList[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse(as), z)((a, b) => f(b, a))

    def appendViaFoldRight[A](as: FunList[A], bs: FunList[A]): FunList[A] =
      foldRightViaFoldLeft(as, bs)((a, acc) => Cons(a, acc))

    def concatViaFoldLeft[A](as: FunList[FunList[A]]): FunList[A] =
      foldLeft(reverseViaFoldLeft(as), Nil: FunList[A])((acc, a) => appendViaFoldRight(a, acc))

    // I actually did not get the foldRight specifics about that and implemented it via foldLeft myself (see above).
    // This link makes clear why to use foldRight: https://www.slideshare.net/RobertoCasadei/functional-programming-in-scala-notes#14
    // Additional note: foldRight is actually not the crucial thing, the crucial thing is to pass the individual lists
    // as the first argument to the append function and not the growing accumulator.
    def concatViaFoldRight[A](as: FunList[FunList[A]]): FunList[A] =
      foldRightViaFoldLeft(as, Nil: FunList[A])((a, acc) => appendViaFoldRight(a, acc))

    def addOneViaFoldRight(l: FunList[Int]): FunList[Int] =
      foldRightViaFoldLeft(l, Nil: FunList[Int])((a, acc) => Cons(a+1, acc))

    def doublesToStringsViaFoldRight(l: FunList[Double]): FunList[String] =
      foldRightViaFoldLeft(l, Nil: FunList[String])((a, acc) => Cons(a.toString, acc))

    def mapViaFoldRight[A, B](as: FunList[A])(f: A => B): FunList[B] =
      foldRightViaFoldLeft(as, Nil: FunList[B])((a, acc) => Cons(f(a), acc))

    def filterViaFoldRight[A](as: FunList[A])(f: A => Boolean): FunList[A] =
      foldRightViaFoldLeft(as, Nil: FunList[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

    def flatMapViaFoldRight[A, B](as: FunList[A])(f: A => FunList[B]): FunList[B] =
      foldRightViaFoldLeft(as, Nil: FunList[B])((a, acc) => appendViaFoldRight(f(a), acc))

    def filterViaFlatMap[A](as: FunList[A])(f: A => Boolean): FunList[A] =
      flatMapViaFoldRight(as)(a => if (f(a)) FunList(a) else Nil)

    def addElements(as: FunList[Int], bs: FunList[Int]): FunList[Int] = {
      @tailrec
      def go(as: FunList[Int], bs: FunList[Int], acc: FunList[Int]): FunList[Int] = as match {
        case Nil          => acc
        case Cons(ha, ta) => bs match {
          case Nil          => acc
          case Cons(hb, tb) => go(ta, tb, Cons(ha + hb, acc))
        }
      }
      reverseViaFoldLeft(go(as, bs, Nil))
    }

    def addElementsViaFoldLeft(as: FunList[Int], bs: FunList[Int]): FunList[Int] = {
      case class Acc(bs: FunList[Int], res: FunList[Int])
      val res = foldLeft(as, Acc(bs, Nil)){ (acc, a) => acc.bs match {
        case Nil          => acc
        case Cons(hb, tb) => Acc(tb, Cons(a + hb, acc.res))
      }}
      reverseViaFoldLeft(res.res)
    }

    def zipWithViaFoldLeft[A](as: FunList[A], bs: FunList[A])(f: (A, A) => A): FunList[A] = {
      case class Acc(bs: FunList[A], res: FunList[A])
      val res = foldLeft(as, Acc(bs, Nil)){ (acc, a) => acc.bs match {
        case Nil          => acc
        case Cons(hb, tb) => Acc(tb, Cons(f(a, hb), acc.res))
      }}
      reverseViaFoldLeft(res.res)
    }

    def hasSubsequence[A](sup: FunList[A], sub: FunList[A]): Boolean = {
      @tailrec
      def go(sup: FunList[A], rem: FunList[A]): Boolean = sup match {
        case Nil => if (lengthViaFoldLeft(rem) == 0) true else false
        case Cons(hSup, tSup) => rem match {
          case Nil              => true
          case Cons(hRem, tRem) => if (hSup == hRem) go(tSup, tRem) else go(tSup, sub)
        }
      }
      go(sup, sub)
    }

  }

  sealed trait Tree[+A]
  case class   Leaf[A](value: A)                        extends Tree[A]
  case class   Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(a)      => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 0
      case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B, C](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)(_ + _ + 1)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(identity)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 0)((l, r) => (l + 1) max (r + 1))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  }

}
