package com.leetcode.problems

object P14 {
  def solution(arr: Array[String]) : String = {
    if (arr == null || arr.length == 0) {
      return ""
    }

    val len = arr(0).length()
    for (i <- 0 until len) {
      val c = arr(0).charAt(i)
      for (j <- 1 until arr.size) {
        if (arr(j).length() == i || c != arr(j).charAt(i)) {
          return arr(0).substring(0, i)
        }
      }
    }
    return ""
  }

  def main(args : Array[String]): Unit = {
    println(solution(Array("lee123455678", "leetcode", "leeyifei")))
  }
}
