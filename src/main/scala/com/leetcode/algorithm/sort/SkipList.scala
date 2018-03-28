package com.leetcode.algorithm.sort

case class SkipList(v: Int, level: Array[SkipList])

object SkipList {
  private val P = 0.5

  var top: SkipList = null;
  var level = 0;

  def init(level:Int): Unit ={
    top = new SkipList(0, new Array[SkipList](level))
  }

  def insert(v:Int):Unit ={
    var x = top
    val tmp = new Array[SkipList](8)
    for (i <- level.to(0, -1)){
      while(x.level(i) != null && x.level(i).v < v){
        x = x.level(i)
      }
      tmp(i) = x
    }
    x = x.level(0)

    if (x ==  null || x.v != v){
      val newLevel = randomLevel
      println(newLevel)
      if (newLevel > level){
        for (i <- level to newLevel)
          tmp(i) = top

        level = newLevel
      }

      val node = new SkipList(v, new Array[SkipList](newLevel + 1))
      for (i <- 0 to newLevel){
        node.level(i) = tmp(i).level(i)
        tmp(i).level(i) = node
      }
    }
  }

  def contains(o: Int) = {
    var x = top
    for (i <- level.to(0, -1)) {
      while (x.level(i) != null && x.level(i).v < o) {
        x = x.level(i)
      }
    }
    x = x.level(0)
    x != null && x.v == o
  }

  def randomLevel = {
    val lvl = (Math.log(1.0 - Math.random()) / Math.log(1 - P)).toInt
    Math.min(lvl, 8)
  }

  def main(args: Array[String]): Unit = {
    init(3)
    insert(5)
    println(contains(5))
  }
}
