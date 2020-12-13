package com.leetcode.offer

/**
  * 输出该链表中倒数第k个结点
  */
object KOfBackwards {
  def kOfBackwards(implicit s:LinkList, k : Int): Node = {
    var p1 = s.head
    var p2 = s.head

    var i = 0
    while (i != k) {
      p2 = p2.next
      i = i + 1
    }

    while (p2 != null) {
      p1 = p1.next
      p2 = p2.next
    }

    p1
  }

  def main(args: Array[String]): Unit = {
    implicit val head = LinkList(Node(1, null))
    head.insert(2)
    head.insert(3)
    head.insert(4)
    head.insert(5)
    head.insert(6)
    head.insert(7)
    head.insert(8)
    head.insert(9)

    head.print()
    val n = kOfBackwards(head, 4)
    print("----------------------\n")
    print(n.key)
  }
}
