package com.leetcode.offer

case class LinkList(key: Int, var next: LinkList)
/**
  * reverse a link list
  */
object ReverseLinkList {
  def insert(linkList: LinkList, key:Int): Unit = {
    var l = linkList
    var p = linkList
    while (l != null) {
      p = l
      l = l.next
    }

    p.next = LinkList(key, null)
  }

  def print(head: LinkList): Unit ={
    var l = head
    while (l != null) {
      System.out.println(l.key)
      l = l.next
    }
  }

  def reverse(head: LinkList): LinkList = {
    var h = head
    var p:LinkList = null
    var n = head.next

    while (n != null) {
      h.next = p
      p = h
      h = n
      n = n.next
    }
    h.next = p
    return h
  }

  def main(args: Array[String]): Unit = {
    val head = LinkList(1, null)
    insert(head, 2)
    insert(head, 3)
    insert(head, 4)
    insert(head, 5)
    insert(head, 6)

    print(head)
System.out.println("-----------------")
    val h = reverse(head)
    print(h)
  }
}
