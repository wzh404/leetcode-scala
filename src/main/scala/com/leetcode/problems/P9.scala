package com.leetcode.problems

/**
  * Determine whether an integer is a palindrome. Do this without extra space.
  */
object P9 {
  /**
    * 反转整数，判断是否相等
    * @param i
    * @return
    */
  def solution(i : Int): Boolean = {
    var sum = 0
    var x = i
    while (x > 0) {
      sum = sum * 10 + x % 10
      x = x / 10
    }

    println(sum + ":" + i)
    sum == i
  }

  def main(args: Array[String]): Unit = {
    solution(123456)
  }
}
