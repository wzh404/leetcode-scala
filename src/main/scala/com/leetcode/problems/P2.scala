package com.leetcode.problems

/**
  * You are given two linked lists representing two non-negative numbers.
  * The digits are stored in reverse order and each of their nodes
  * contain a single digit. Add the two numbers and return it as a linked list.
  *
  * Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
  * Output: 7 -> 0 -> 8
  *
  * @author wangzunhui
  */
object P2 {
  def addTwoNumbers(l1 : List[Int], l2: List[Int]) : List[Int] = {
    var result: List[Int] = List()
    var i = l1.length - 1;
    var j = l2.length - 1;
    var carry = 0;

    while (j >=0 || i >= 0){
      val v1:Option[Int] = l1.lift(i) match {
        case None => Option(0)
        case y:Option[Int] => y
      }

      val v2:Option[Int] = l2.lift(j) match {
        case None => Option(0)
        case y:Option[Int] => y
      }

      val sum = v1.get + v2.get + carry;
      carry = sum / 10
      val v = sum - carry * 10;
      result = result :+ v;

      i = i - 1;
      j = j - 1;
    }
    if (carry == 1){
      result = result :+ 1
    }
    result
  }

  def main(args: Array[String]) = {
    val l1 = List(2,4,3);
    val l2 = List(5,6,4);
    println(addTwoNumbers(l1, l2))
  }
}
