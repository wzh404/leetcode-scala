package com.leetcode.algorithm.sort

/**
  * one pivot two-way scan quick sort.
  */
object OnePivotTwoWayScanQuickSort {
  def partition(list:Array[Int], left : Int, right:Int) : Int = {
    val base:Int = list(left);
    var i = left;
    var j:Int = right;

    while(i < j){
      while(i < j && list(j) >= base){
        j = j - 1;
      }
      if (i < j){
        list(i) = list(j);
        i = i + 1;
      }

      while(i < j && list(i) < base){
        i = i + 1;
      }
      if (i < j){
        list(j) = list(i);
        j = j - 1;
      }
    }

    list(i) = base;
    i
  }

  def sort(list:Array[Int], left: Int, right:Int): Unit ={
    if (left < right){
      val i:Int = partition(list, left, right)
      sort(list, left, i - 1)
      sort(list, i + 1, right)
    }
  }

  def main(args: Array[String]): Unit ={
    val l:Array[Int] = Array(72,6,57,88,60,42,83,73,48,85)
    sort(l, 0, l.size - 1)
    println(l.mkString(","))
  }
}
