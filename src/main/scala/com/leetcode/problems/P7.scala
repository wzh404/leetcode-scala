package com.leetcode.problems

object P7 {
  def solution(i: Int) : Int = {
    var x = i
    var res = 0
    while (x > 0) {
      res = res * 10 + x % 10
      x = x / 10
    }

    println(res)
    return res
  }

  def main(args: Array[String]): Unit ={
    solution(497939373)
  }
}
