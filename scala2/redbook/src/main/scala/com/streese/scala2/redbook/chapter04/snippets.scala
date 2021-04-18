package com.streese.scala2.redbook.chapter04

object snippets {

  sealed trait FunOption[+A] {
    def map[B](f: A => B): FunOption[B]
    def flatMap[B](f: A => FunOption[B]): FunOption[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => FunOption[B]): FunOption[B]
    def filter(f: A => Boolean): FunOption[A]
  }

  case class Some[+A](get: A) extends FunOption[A] {

    def map[B](f: A => B): FunOption[B] = Some(f(this.get))

    def flatMap[B](f: A => FunOption[B]): FunOption[B] = f(this.get)

    def getOrElse[B >: A](default: => B): B = this.get

    def orElse[B >: A](ob: => FunOption[B]): FunOption[B] = this

    def filter(f: A => Boolean): FunOption[A] = if (f(this.get)) this else None

  }

  case object None extends FunOption[Nothing] {

    def map[B](f: Nothing => B): FunOption[B] = this

    def flatMap[B](f: Nothing => FunOption[B]): FunOption[B] = this

    def getOrElse[B](default: => B): B = default

    def orElse[B](ob: => FunOption[B]): FunOption[B] = ob

    def filter(f: Nothing => Boolean): FunOption[Nothing] = this

  }

  def mean(xs: Seq[Double]): FunOption[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): FunOption[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A,B](f: A => B): FunOption[A] => FunOption[B] = _ map f

  def map2[A,B,C](a: FunOption[A], b: FunOption[B])(f: (A, B) => C): FunOption[C] = a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](as: List[FunOption[A]]): FunOption[List[A]] =
    as.foldRight(Some(List.empty): FunOption[List[A]])((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => FunOption[B]): FunOption[List[B]] =
    as.foldRight(Some(List.empty): FunOption[List[B]])((a, acc) => f(a).flatMap(x => acc.map(xs => x :: xs)))

  def traverse2[A, B](as: List[A])(f: A => FunOption[B]): FunOption[List[B]] =
    as.foldRight(Some(List.empty): FunOption[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequenceViaTraverse[A](as: List[FunOption[A]]): FunOption[List[A]] = traverse[FunOption[A], A](as)(identity)

  sealed trait FunEither[+E, +A] {
    def map[B](f: A => B): FunEither[E, B]
    def flatMap[EE >: E, B](f: A => FunEither[EE, B]): FunEither[EE, B]
    def orElse[EE >: E, B >: A](b: => FunEither[EE, B]): FunEither[EE, B]
    def map2[EE >: E, B, C](b: FunEither[EE, B])(f: (A, B) => C): FunEither[EE, C]
  }

  case class Left[+E](value: E) extends FunEither[E, Nothing] {
    def map[B](f: Nothing => B): FunEither[E,B] = this
    def flatMap[EE >: E, B](f: Nothing => FunEither[EE,B]): FunEither[EE,B] = this
    def orElse[EE >: E, B](b: => FunEither[EE,B]): FunEither[EE,B] = b
    def map2[EE >: E, B, C](b: FunEither[EE,B])(f: (Nothing, B) => C): FunEither[EE,C] = this
  }

  case class Right[+A](value: A) extends FunEither[Nothing, A] {
    def map[B](f: A => B): FunEither[Nothing,B] = Right(f(this.value))
    def flatMap[EE, B](f: A => FunEither[EE,B]): FunEither[EE,B] = f(this.value)
    def orElse[EE, B >: A](b: => FunEither[EE,B]): FunEither[EE,B] = this
    def map2[EE, B, C](b: FunEither[EE,B])(f: (A, B) => C): FunEither[EE,C] = this.flatMap(x => b.map(y => f(x, y)))
  }

  def sequence[E, A](es: List[FunEither[E, A]]): FunEither[E, List[A]] =
    es.foldRight(Right(List.empty): FunEither[E, List[A]])((x, acc) => x.map2(acc)(_ :: _))

  def traverse[E, A, B](es: List[A])(f: A => FunEither[E, B]): FunEither[E, List[B]] =
    es.foldRight(Right(List.empty): FunEither[E, List[B]])((x, acc) => f(x).map2(acc)(_ :: _))

  def sequenceViaTraverse[E, A](es: List[FunEither[E, A]]): FunEither[E, List[A]] =
    traverse[E, FunEither[E, A], A](es)(identity)

}
