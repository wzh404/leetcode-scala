package com.leetcode.problems

object P11 {
  def solution(height: Array[Int]): Int = {
    var start: Int = 0
    var end: Int = height.length - 1
    var maxArea : Int = 0

    while (start < end) {
      maxArea = math.max(maxArea, (end - start) * math.min(height(start), height(end)))
      if (height(start) < height(end)) {
        start = start + 1
      } else {
        end = end - 1
      }
    }
    maxArea
  }

  def main(args : Array[String]): Unit = {
    val area = solution(Array(1,8,6,2,5,4,8,3,7))
    println(area)
  }
}
