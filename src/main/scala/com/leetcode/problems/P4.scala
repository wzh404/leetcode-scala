package com.leetcode.problems

/**
  * There are two sorted arrays nums1 and nums2 of size m and n respectively.
  *
  * Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
  *
  * Example 1:
  *
  *   nums1 = [1, 3]
  *   nums2 = [2]
  *   The median is 2.0
  * Example 2:
  *
  *   nums1 = [1, 2]
  *   nums2 = [3, 4]
  *   The median is (2 + 3)/2 = 2.5
  */
object P4 {
  def median(a: Array[Int], b: Array[Int]): Double = {
    var a1: Int = 0
    var a2: Int = a.length - 1
    var b1: Int = 0
    var b2: Int = b.length - 1
    val even: Boolean = (a.length + b.length) % 2 == 0
    val p = (a.length + b.length) / 2

    var medianTotalValue: Int = 0
    var ok : Boolean = true
    while(ok) {
      val ma = a1 + (a2 - a1) / 2
      val mb = b1 + (b2 - b1) / 2
      val left : Int = ma + mb

      if (p == left) {
        if (even) {
          if (ma > 0 && mb > 0) {
            val v = if (a(ma - 1) > b(mb - 1)) a(ma - 1) else b(mb - 1)
            println(v)
            medianTotalValue += v
          } else if (ma > 0) {
            println(a(ma - 1))
            medianTotalValue += a(ma - 1)
          } else if (mb > 0) {
            println(b(mb - 1))
            medianTotalValue += b(mb - 1)
          }
        }
        val v = if (a(ma) > b(mb)) b(mb) else a(ma)
        println(v)
        medianTotalValue += v
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

    if (even) medianTotalValue/2.0 else medianTotalValue/1.0
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Int] = Array(2, 4, 7, 11, 14, 20,23,34)
    val b = Array(1, 5, 8, 9, 19, 21)

//    val a: Array[Int] = Array(1,3)
//    val b = Array(2,4)

    println(median(a, b))
  }
}