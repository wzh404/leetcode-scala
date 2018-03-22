package com.leetcode.algorithm.sort

import com.leetcode.algorithm.sort.OnePivotD3ScanQuickSort.sort

object OnePivotD3TwoWayScanQuickSort {
  def sort(a: Array[Int], l: Int, r: Int): Unit = {
    if (l >= r) return;
    var i = l;
    var k = i + 1;
    var j = r;

    val pivot = a(l);
    var scanned = false;
    while (k <= j && !scanned) {
      if (a(k) < pivot){
        swap(a, i, k);
        k = k + 1;
        i = i + 1;
      }
      else if (a(k) == pivot){
        k = k + 1;
      }
      else { // two-way
        while(a(j) > pivot && !scanned){
          j = j - 1;
          if (j < k) {
            scanned = true
          }
        }

        if (!scanned){
          if (a(j) < pivot){
            swap(a, k, j);
            swap(a, k, i);

            i = i + 1;
          }
          else {
            swap(a, k,j)
          }
          k = k + 1;
          j = j - 1;
        }
      }
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
