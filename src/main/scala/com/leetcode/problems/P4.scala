package com.leetcode.problems

object P4 {
  def mean(a: Array[Int], b: Array[Int]): Double = {
    var a1: Int = 0
    var a2: Int = a.length - 1
    var b1: Int = 0
    var b2: Int = b.length - 1
    val even: Boolean = (a.length + b.length) % 2 == 0
    val p = (a.length + b.length) / 2

    var ok : Boolean = true
    while(ok) {
      val ma = a1 + (a2 - a1) / 2
      val mb = b1 + (b2 - b1) / 2
      val left : Int = ma + mb

      if (p == left) {
        if (a(ma) > b(mb)) {
          println(b(mb))
        } else {
          println(a(ma))
        }
        ok = false
      } else if (p > left) {
        if (a(ma) < b(mb)) {
          a1 = ma
        } else {
          b1 = mb
        }
      } else {
        if (a(ma) < b(mb)) {
          b2 = mb
        } else {
          a2 = ma
        }
      }
    }
    return 0
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Int] = Array(2, 4, 7, 11, 14, 20,23,34)
    val b = Array(1, 5, 8, 9, 19, 21)

    mean(a, b)
  }
}