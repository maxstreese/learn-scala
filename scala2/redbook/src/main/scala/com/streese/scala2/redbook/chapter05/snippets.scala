package com.streese.scala2.redbook.chapter05

import scala.annotation.tailrec

object snippets {

  sealed trait Stream[+A] {

    import Stream._

    def headOption: Option[A] = this match {
      case Cons(h, _) => Some(h())
      case _          => None
    }

    def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((a, acc) => Some(a))

    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case _          => List.empty
    }

    def toList: List[A] = {
      @tailrec
      def go(s: Stream[A], l: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: l)
        case _          => l
      }
      go(this, List.empty).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) => if (n < 1) empty else cons(h(), t().take(n - 1))
      case _          => empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) => if (n < 1) this else t().drop(n - 1)
      case _          => empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
      case _          => empty
    }

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty: Stream[A])((a, acc) => if(p(a)) cons(a, acc) else empty)

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, acc) => p(a) || acc)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

    def map[B](f: A => B): Stream[B] = foldRight(empty: Stream[B])((a, acc) => cons(f(a), acc))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty: Stream[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

    def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty: Stream[B])((a, acc) => f(a).append(acc))

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this)( _ match {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    })

    def takeViaUnfold(n: Int): Stream[A] = unfold(this)(_ match {
      case Cons(h, t) => if (n < 1) None else Some((h(), t().takeViaUnfold(n-1)))
      case _          => None
    })

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this)(_ match {
      case Cons(h, t) => if (p(h())) Some((h(), t().takeWhileViaUnfold(p))) else None
      case _          => None
    })

    def zipWithViaUnfold[B >: A](that: Stream[B])(f: (B, B) => B): Stream[B] = unfold((this, that)) { case (s1, s2) =>
      s1 match {
        case Cons(h1, t1) => s2 match {
          case Cons(h2, t2) => Some((f(h1(), h2())), (t1(), t2()))
          case _            => None
        }
        case _ => None
      }
    }

    def zipAllViaUnfold[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, that)) { case (s1, s2) =>
      s1 match {
        case Cons(h1, t1) => s2 match {
          case Cons(h2, t2) => Some((Some(h1()), Some(h2())), (t1(), t2()))
          case _            => Some((Some(h1()), None), (t1(), empty))
        }
        case _ => s2 match {
          case Cons(h2, t2) => Some((None, Some(h2())), (empty, t2()))
          case _            => None
        }
      }
    }

    def startsWith[A](s: Stream[A]): Boolean = zipAllViaUnfold(s).takeWhile(!_._2.isEmpty).forAll {
      case (h,h2) => h == h2
    }

    def tails: Stream[Stream[A]] = unfold(this)(s => s match {
      case c: Cons[A] => Some(c, c.t())
      case _          => None
    }).append(Stream.empty)

    /**
      * This implementation will not utilize the laziness of foldRight because the function passed to foldRight will
      * always evaluate both of its parameters. Therefore this implementation is strict and not lazy.
      */
    def scanRightNonLazy[B](z: => B)(f: (A, => B) => B): Stream[B] =
      foldRight((Stream(z), z)){ case (a, (bs, b)) =>
        val newB = f(a, b)
        cons(newB, bs) -> newB
      }._1

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((Stream(z), z))((a, acc) => {
        lazy val acc1 = acc
        val newB = f(a, acc1._2)
        (cons(newB, acc1._1), newB)
      })._1

    def scanRightWhyNotLazy[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((Stream(z), z)) { case (a, acc) => {
        lazy val acc1 = acc
        val newB = f(a, acc1._2)
        (cons(newB, acc1._1), newB)
      }}._1

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    def fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }

    def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).fold(empty: Stream[A]){ case (a, s) => cons(a, unfold(s)(f)) }

    def onesViaUnfold: Stream[Int] = unfold(1)(s => Some(s, s))

    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

    def fibsViaUnfold: Stream[Int] = unfold((0, 1)){ case (a, b) => Some(a, (b, a+b)) }

  }

}
