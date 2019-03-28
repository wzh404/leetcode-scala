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
        if (even) {
          if (ma > 0 && mb > 0) {
            if (a(ma - 1) > b(mb - 1)) println(a(ma - 1)) else println(b(mb - 1))
          } else if (ma > 0) {
            println(a(ma - 1))
          } else if (mb > 0) {
            println(b(mb - 1))
          }
        }
        if (a(ma) > b(mb)) println(b(mb)) else println(a(ma))
        ok = false
      } else if (p > left) {
        if (a(ma) < b(mb)) {
          if (ma < a2) {
            a1 = ma + 1
          } else {
            b1 = mb + 1
          }
        } else {
          if (mb < b2) {
            b1 = mb + 1
          } else {
            a1 = ma + 1
          }
        }
      } else {
        if (a(ma) < b(mb)) {
          if (mb > 0) {
            b2 = mb - 1
          } else {
            a2 = ma - 1
          }
        } else {
          if (ma > 0) {
            a2 = ma - 1
          } else {
            b2 = mb - 1
          }
        }
      }
    }
    return 0
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Int] = Array(2, 4, 7, 11, 14, 20,23,34)
    val b = Array(1, 5, 8, 9, 19, 21)

//    val a: Array[Int] = Array(1,3)
//    val b = Array(2,4)

    mean(a, b)
  }
}