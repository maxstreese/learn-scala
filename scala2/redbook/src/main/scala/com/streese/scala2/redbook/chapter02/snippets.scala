package com.streese.scala2.redbook.chapter02

object snippets {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

}
