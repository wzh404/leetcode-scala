package com.leetcode.problems

import scala.collection.immutable.BitSet

/**
  * 给定一个字符串，找出不含有重复字符的最长子串的长度。
  *
  * "abcabcbb"  abc = 3
  * "bbbbbbbb"  b = 1
  */
object P3 {
  var bs = BitSet(0)

  def lengthOfLongestSubstring(s: String): Int = {
    var i = 0
    var j = 0
    var max = 1

    while (j != s.length) {
      in(s.charAt(j)) match {
        case true => {
          val m = j - i - 1
          if (m > max) max = m

          i = i + 1
          bs -= s.charAt(j).toInt
        }
        case false => bs += s.charAt(j).toInt
      }

      j = j + 1
    }

    max
  }

  def in(c: Char): Boolean = {
    bs.contains(c.toInt)
  }

  def main(args: Array[String]) = {
    println("length is " + lengthOfLongestSubstring("abcabcdbb"))
  }
}
