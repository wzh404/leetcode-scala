package com.leetcode.algorithm.sort

case class SkipList(v: Int, down: SkipList, next: SkipList)

object SkipList {
  var top: SkipList = null;

  def insert(v: Int): Unit = {
    if (top == null) {
      top = new SkipList(v, null, null);
    }
  }
}
