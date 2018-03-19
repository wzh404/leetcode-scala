package com.leetcode.problems

import scala.collection.mutable.Map

/**
  * Given an array of integers, return indices of the two numbers such that they add up to a specific target.
  *
  * You may assume that each input would have exactly one solution.
  *
  * Example:
  *
  * Given nums = [2, 7, 11, 15], target = 9,
  *
  * Because nums[0] + nums[1] = 2 + 7 = 9,
  * return [0, 1].
  *
  */
object P1 {
  def twoSum(nums : Array[Int], target: Int) : Set[(Int, Int)] = {
    val m : Map[Int, Int] = Map();
    var s : Set[(Int, Int)] = Set();

    for (i <- 0 until nums.size){
      m(nums(i)) = i;
    }

    for (i <- 0 until nums.size){
      val n = target - nums(i);
      if (m.contains(n) && m(n) != i){
        s +=((i, m(n)))
        return s
      }
    }
    s
  }

  def main(args: Array[String]) = {
    println(twoSum(Array(2, 7, 11, 15), 9).head);
  }
}
