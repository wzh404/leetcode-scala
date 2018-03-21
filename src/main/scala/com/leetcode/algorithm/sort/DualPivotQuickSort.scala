package com.leetcode.algorithm.sort

/**
  * dual pivot two-way scan quick sort
  *
  * @author wangzunhui
  */
object DualPivotQuickSort {
  def sort(a: Array[Int], l: Int, r: Int): Unit = {
    if (r < l) return;
    if (a(l) > a(r)) {
      swap(a, l, r)
    }

    val pivot1: Int = a(l);
    val pivot2: Int = a(r);
    var i: Int = l;
    var j: Int = r;
    var k: Int = l + 1;

    var scanned = false;
    while (k < j && !scanned) {
      if (a(k) < pivot1) {
        i = i + 1;
        swap(a, k, i);
        k = k + 1;
      }
      else if (a(k) <= pivot2) {
        k = k + 1;
      }
      else {
        j = j - 1;
        while (a(j) > pivot2 && !scanned) {
          j = j - 1;
          if (j <= k) {
            scanned = true
          };
        }

        if (!scanned) {
          if (a(j) < pivot1) {
            swap(a, k, j)
            i = i + 1
            swap(a, k, i)
          }
          else {
            swap(a, j, k);
          }
          k = k + 1;
        }
      }
    }

    // a(i) = pivot1
    swap(a, l, i);
    // a(j) = pivot2
    swap(a, j, r);

    sort(a, l, i - 1);
    sort(a, i + 1, j - 1);
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
