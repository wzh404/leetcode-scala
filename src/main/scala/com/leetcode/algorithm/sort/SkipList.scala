package com.leetcode.algorithm.sort

case class SkipList(v: Int, down: Array[SkipList], next: SkipList)

object SkipList {
  private val P = 0.5

  var top: SkipList = null;
  var level = 0;

  def init(level:Int): Unit ={
    top = new SkipList(null, new Array(level), null)
  }

  def search(v:Int):SkipList ={
    var skip = false

    var x = top
    var tmp = new Array[SkipList](8)
    for (i <- level.to(0, -1)){
      while(x.down(i) != null && x.down(i).v < v){
        x = x.down(i)
      }
      tmp(i) = x
    }
    x = x.down(0)



    // to down


    return null;
  }

  def insert(v: Int): Unit = {

  }

  def randomLevel = {
    val lvl = (Math.log(1.0 - Math.random()) / Math.log(1 - P)).toInt
    Math.min(lvl, 8)
  }

  def main(args: Array[String]): Unit = {
    println(randomLevel)
  }
}
