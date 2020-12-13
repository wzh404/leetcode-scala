package com.leetcode.fp.book

object Fn2$1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int = {
      if (n == 0)
        current
      else
        return go(n - 1, next, next+current)
    }

    go(n, 0, 1)
  }
}
