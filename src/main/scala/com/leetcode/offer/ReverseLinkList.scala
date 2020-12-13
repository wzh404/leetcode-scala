package com.leetcode.offer

/**
  * 反转链表
  * @param head
  */
case class LinkList(head: Node) {
  def insert(key:Int): Unit = {
    var l = this.head
    var p = this.head
    while (l != null) {
      p = l
      l = l.next
    }

    p.next = Node(key, null)
  }

  def print(): Unit ={
    var l = this.head
    while (l != null) {
      System.out.println(l.key)
      l = l.next
    }
  }

  def reverse(): LinkList = {
    var h = head
    var p:Node = null
    var n = head.next

    while (n != null) {
      h.next = p
      p = h
      h = n
      n = n.next
    }
    h.next = p
    return LinkList(h)
  }
}

case class Node(key: Int, var next: Node)
/**
  * reverse a link list
  */
object ReverseLinkList {
  def main(args: Array[String]): Unit = {
    val head = LinkList(Node(1, null))
    head.insert(2)
    head.insert(3)
    head.insert(4)
    head.insert(5)
    head.insert(6)

    head.print()
    System.out.println("-----------------")
    val h = head.reverse()
    h.print()
  }
}
