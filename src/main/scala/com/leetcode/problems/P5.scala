package com.leetcode.problems

/**
  * Given a string s, find the longest palindromic substring in s.
  * You may assume that the maximum length of s is 1000.
  *
  * Example:
  *
  * Input: "babad"
  * Output: "bab"
  */
object P5 {
  /**
    * dp[i..j] = 1 is palindromic string
    *
    * if (s[i+1:j-1]) == 1 && s[i] == s[j] then
    *   s[i][j] = 1
    * else
    *   s[i][j] = 0
    *
    * @param s
    * @return
    */
  def dynamicProgrammingSolution(s: String): Int = {
    if (s == null || s.length() == 0) return 0
    val len = s.length
    if (len == 1) return 1
    var longest = 0
    val dp = Array.ofDim[Int](len + 1, len + 1)

    for (i <- 0 to len - 2) {
      dp(i)(i) = 1
      if (s(i) == s(i + 1)) {
        dp(i)(i + 1) = 1
      }
    }

    for (i <- 0 to len - 3) {
      for (j <- i + 2 to len - 1) {
        if (s(i) == s(j)) {
          dp(i)(j) = dp(i + 1)(j - 1)
          if (dp(i)(j) == 1 && longest < j - i + 1) {
            println(i + ":" + j)
            longest = j - i + 1
          }
        }
      }
    }
    println("longest is " + longest)
    return longest
  }

  def main(args: Array[String]): Unit = {
    dynamicProgrammingSolution("babad")
  }
}
