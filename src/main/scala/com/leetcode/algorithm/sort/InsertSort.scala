package com.leetcode.algorithm.sort

object InsertSort {
  def sort(a: Array[Int], f: (Int, Int) => Boolean): Unit = {
    for (i <- 1 to a.length - 1) {
      val key = a(i)
      var l = i

      var find = true;
      for (j <- 0 to i - 1) {
        if (f(a(j), key) && find) {
          l = j
          find = false
        }
      }

      if (l < i) {
        for (k <- Range(i, l, -1)) {
          a(k) = a(k - 1)
        }
        a(l) = key
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val l: Array[Int] = Array(31,41,59,26,41,58)
    sort(l, (x, y) => x > y)
    println(l.mkString(","))
  }
}
