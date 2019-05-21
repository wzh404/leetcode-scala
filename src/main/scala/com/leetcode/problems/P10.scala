package com.leetcode.problems

/**
  * Given an input string (s) and a pattern (p), implement regular expression matching with support for '.' and '*'.
  *
  * '.' Matches any single character.
  * '*' Matches zero or more of the preceding element.
  * The matching should cover the entire input string (not partial).
  *
  * Note:
  *
  * s could be empty and contains only lowercase letters a-z.
  * p could be empty and contains only lowercase letters a-z, and characters like . or *.
  */
object P10 {
  def solution(s: String, p : String) : Boolean = {
    if (p.isEmpty()) return s.isEmpty()

    val firstMatch = (!s.isEmpty) && (s.charAt(0) == p.charAt(0) || p.charAt(0) == '.')

    if (p.length >= 2 && p.charAt(1) == '*') {
      solution(s.substring(1), p.substring(2)) || (firstMatch && solution(s.substring(1), p))
    } else {
      firstMatch && solution(s.substring(1), p.substring(1))
    }
  }

  def dp(s : String, p : String) : Boolean = {

    false
  }

  def main(args: Array[String]): Unit = {
    println(solution("mississippi", "mis*is*p*."))
  }
}
