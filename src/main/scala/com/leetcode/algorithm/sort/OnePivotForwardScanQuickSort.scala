package com.leetcode.algorithm.sort

object OnePivotForwardScanQuickSort {
  def forwardScan(a: Array[Int], left: Int, right: Int): Unit ={
    if (left >= right) return;

    var i = left;
    var j = left + 1;
    val pivot: Int = a(i);

    while(j < right){
      if (a(j) < pivot){
        swap(a, i, j)
        i = i + 1;
      }
      j = j + 1;
    }
    swap(a, left, i);
    forwardScan(a, left, i - 1)
    forwardScan(a, i + 1, right)
  }

  def swap(a: Array[Int], i: Int, j:Int): Unit = {
    val t = a(i);
    a(i) = a(j)
    a(j) = t
  }
}
