package com.leetcode.problems

/**
  * The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows
  * like this: (you may want to display this pattern in a fixed font for better legibility)
  *
  * P   A   H   N
  * A P L S I I G
  * Y   I   R
  *
  * And then read line by line: "PAHNAPLSIIGYIR" Write the code that will take a string
  * and make this conversion given a number of rows:
  */
object P6 {
  def convert(text: String, nRows: Int): Unit = {
    val step = 2 * nRows - 2
    val len = text.length()
    var i: Int = 0

    while (len > i) {
      print(text.charAt(i))
      i = i + step
    }

    for (m <- 1 to nRows - 2) {
      var l: Int = 0
      while (len > l) {
        print(text.charAt(l + m))
        l = l + step - m
        if (len > l) {
          print(text.charAt(l))
        }
        l = l + m
      }
    }

    var k: Int = nRows - 1
    while (len > k) {
      print(text.charAt(k))
      k = k + step
    }

    println("")
  }

  def main(args: Array[String]): Unit = {
    convert("PAYPALISHIRING", 3)
  }
}
