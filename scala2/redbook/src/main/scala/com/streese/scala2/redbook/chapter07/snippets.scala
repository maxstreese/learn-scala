package com.streese.scala2.redbook.chapter07

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object snippets {

  object Par {

    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = es => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone: Boolean = true
      def get(timeout: Long, unit: TimeUnit): A = get
      def isCancelled: Boolean = false
      def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

    case class Map2Future[A,B,C](
      a: Future[A],
      b: Future[B],
      f: (A,B) => C
    ) extends Future[C] {

      @volatile var cache: Option[C] = None

      def isDone                                 = cache.isDefined
      def isCancelled                            = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean)         = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get                                    = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }

    }

    def map2WithTimeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

    def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
      def call = a(es).get
    })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork(sequence(ps.map(asyncF(f))))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[Option[A]]] = as.map(asyncF((a: A) => if (f(a)) Some(a) else None))
      map(sequence(pars))(_.flatten)
    }

  }

}
