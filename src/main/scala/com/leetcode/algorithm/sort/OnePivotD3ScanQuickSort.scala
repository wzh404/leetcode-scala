package com.leetcode.algorithm.sort

import com.leetcode.algorithm.sort.OnePivotTwoWayScanQuickSort.sort

/**
  * one pivot division three one-way scan quick sort.
  */
object OnePivotD3ScanQuickSort {
  def sort(a: Array[Int], l: Int, r: Int): Unit = {
    if (l >= r) return;

    var i = l;
    var k = i + 1;
    var j = r;

    val pivot = a(l);
    while (k <= j) {
      if (a(k) > pivot) {
        swap(a, k, j);
        j = j - 1;
      }
      else if (a(k) < pivot) {
        swap(a, i, k);
        i = i + 1;
      }

      k = k + 1;
    }

    sort(a, l, i - 1);
    sort(a, j + 1, r);
  }

  def swap(a: Array[Int], i: Int, j: Int): Unit = {
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
