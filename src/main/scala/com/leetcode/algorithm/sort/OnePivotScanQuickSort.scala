package com.leetcode.algorithm.sort

/**
  * one pivot one-way scan quick sort.
  */
object OnePivotScanQuickSort {
  def sort(a: Array[Int], left: Int, right: Int): Unit ={
    if (left >= right) return;

    var i = left;
    var j = left + 1;
    val pivot: Int = a(i);

    while(j <= right){
      if (a(j) < pivot){
        i = i + 1;
        swap(a, i, j)
      }
      j = j + 1;
    }
    swap(a, left, i);
    sort(a, left, i - 1)
    sort(a, i + 1, right)
  }

  def swap(a: Array[Int], i: Int, j:Int): Unit = {
    val t = a(i);
    a(i) = a(j)
    a(j) = t
  }

  def main(args: Array[String]): Unit = {
    val l: Array[Int] = Array(72, 6, 57, 88, 60, 42, 83, 73, 48, 85)
    sort(l, 0, l.size - 1)
    println(l.mkString(","))
  }
}
