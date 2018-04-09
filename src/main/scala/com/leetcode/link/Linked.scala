package com.leetcode.link

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Random

case class Route(id: String, clz: Int, other: String)

case class Node(id: String, clz: Int, other: String, var alive: Boolean) {
  var routes: Array[Route] = Array()
  var data: mutable.HashMap[String, Seq[Int]] = mutable.HashMap()
  var _nextData = 1

  def addRoute(route: Route): Unit = {
    routes = routes :+ route
  }

  def offline(): Unit = {
    this.alive = false;
    println(nodeName() + " offline");
  }

  def online(): Unit = {
    if (alive) return

    println(nodeName() + " online");
    requestAndRefreshRoute()
    requestAndRefreshData()

    this.alive = true;
  }

  def printRoutes(): Unit = {
    println(nodeName() + " - " + this.routes.mkString(","))
  }

  def printData(): Unit = {
    println(nodeName() + " - " + this.data)
  }

  def nodeName(): String = {
    "[" + this.id + ":" + this.clz + ":" + (if (this.alive) "on" else "off") + "]"
  }

  /**
    * Save the published data.
    *
    * @param id node id.
    * @param data
    */
  def saveData(id: String, data: Int): Unit = {
    var seq = Seq[Int]()
    if (this.data.contains(id)) {
      seq = this.data(id)
    }
    seq +:= data
    this.data += (id -> seq)
  }

  /**
    * Receive the request and save data.
    *
    * @param requestNode request node
    * @param originNode  origin node
    * @param data        data to be published from origin node
    */
  def receiveNewData(requestNode: Node, originNode: Node, data: Int): Unit = {
    if (!alive) return;

    saveData(originNode.id, data)
    if (requestNode.clz != this.clz) {
      val sameNodes = getSameNodes()
      println(nodeName() + " transmit data to " + sameNodes.mkString(","))
      sameNodes.foreach(n => n.receiveNewData(this, originNode, data))
    }
  }

  /**
    * Receive the request and add the node to the routing table.
    *
    * @param reqNode request node.
    * @param newNode new node
    */
  def receiveNewNode(reqNode: Node, newNode: Node): Unit = {
    if (!alive) return;

    val route = Route(newNode.id, newNode.clz, newNode.other)
    addRoute(route)

    // transmit
    if (this.clz != reqNode.clz) {
      val sameNodes = getSameNodes()
      println(nodeName() + " transmit new node to " + sameNodes.mkString(","))
      sameNodes.foreach(n => n.receiveNewNode(this, newNode))
    }
  }

  /**
    * Receive the request and return the routing table.
    *
    * @param reqNode request node.
    * @return
    */
  def receiveAndReturnRoutes(reqNode: Node): Array[Route] = {
    var list = ListBuffer[Route]()

    list ++= routes.filter(r => !r.id.eq(reqNode.id))
    list += Route(this.id, this.clz, this.other)
    list.toArray
  }

  /**
    * Receive the request and returns data not received by the request node
    *
    * @param requestNode Request node.
    * @param requestData The maximum data received by the request node.
    * @return The data not received by the request node.
    */
  def receiveAndReturnData(requestNode: Node, requestData: Map[String, Int]): Map[String, Seq[Int]] = {
    requestData.filter(r => this.data.contains(r._1))
      .map(r => (r._1 -> this.data(r._1).filter(p => p > r._2)))
  }

  /**
    * randomly acquire a node from the routing table
    *
    * @return
    */
  def getNode(): Node = {
    val r = Random.nextInt(routes.length)

    Linked.find(routes(r).id)
  }

  /**
    * randomly acquire same node from the routing table.
    *
    * @return
    */
  def getSameNode(): Node = {
    val sameNodes = getSameNodes();
    val r = Random.nextInt(sameNodes.length)

    sameNodes(r)
  }

  /**
    * randomly acquire same nodes from the routing table.
    *
    * @return
    */
  def getSameNodes(): Array[Node] = {
    routes.filter(r => r.clz == this.clz)
      .map(r => Linked.find(r.id))
  }

  /**
    * randomly acquire other node from the routing table.
    */
  def getOtherNode(otherClz: Int): Option[Node] = {
    val list = routes.filter(r => r.clz == otherClz)
      .map(r => Linked.find(r.id))
      .filter(n => n.alive)

    if (list.isEmpty) {
      Option.empty
    } else {
      Option(list(Random.nextInt(list.length)))
    }
  }

  /**
    * randomly acquire other nodes from the routing table.
    */
  def getOtherNodes(): Array[Node] = {
    Linked.allClz.filter(i => i != this.clz)
      .map(i => {
        getOtherNode(i)
      })
      .filter(o => o != Option.empty)
      .map(o => o.get)
  }

  /**
    * Refresh routing table.
    */
  def requestAndRefreshRoute(): Unit = {
    var loop = true
    while (loop) {
      val node = getNode()
      if (node.alive) {
        println(nodeName() + " refresh route from " + node.id)
        loop = false
        this.routes = node.receiveAndReturnRoutes(this)
      }
    }
  }

  /**
    * Save latest data by (k,v)
    *
    * @param k node id
    * @param v data
    */
  def saveLatestData(k: String, v: Seq[Int]): Unit = {
    if (data.contains(k)) {
      this.data(k) ++= v
    } else {
      this.data(k) = v
    }
  }

  /**
    * refresh latest data.
    */
  def requestAndRefreshData(): Unit = {
    var loop = true
    while (loop) {
      val node = getNode()
      if (node.alive) {
        val requestData = this.getLatestData()
        val latestData = node.receiveAndReturnData(this, requestData)
        println(nodeName() + " request latest data - " + requestData)
        println(nodeName() + " response latest data - " + latestData)

        latestData.foreach(d => saveLatestData(d._1, d._2))
        loop = false
      }
    }
  }

  /**
    * Gets the latest data for each node received.
    *
    * @return
    */
  def getLatestData(): Map[String, Int] = {
    return routes.map(r => (r.id -> (if (data.contains(r.id)) data(r.id).max else 0))).toMap
  }

  /**
    * Request to add a new node.
    */
  def requestAddNode(): Unit = {
    val sameNodes = this.getSameNodes()
    for (n <- sameNodes) {
      n.receiveNewNode(this, this)
    }

    val otherNodes = this.getOtherNodes()
    for (n <- otherNodes) {
      n.receiveNewNode(this, this)
    }
  }

  /**
    * Publish new data.
    */
  def publishData(): Unit = {
    if (!alive) {
      println(nodeName() + " is offline")
      return
    }


    println(nodeName() + " publish data " + _nextData)
    saveData(this.id, _nextData)

    // transmit to same nodes
    val sameNodes = this.getSameNodes()
    for (n <- sameNodes) {
      n.receiveNewData(this, this, _nextData)
    }

    // transmit to other nodes
    val otherNodes = this.getOtherNodes()
    for (n <- otherNodes) {
      n.receiveNewData(this, this, _nextData)
    }

    _nextData += 1
  }
}

object Linked {
  val allClz = Array(1, 2, 3)
  var staticRoutes = Array[Route]()

  var initNodes = ListBuffer[Node](
    Node("N1", 1, "N1:8081", true),
    Node("N2", 2, "N2:8082", true),
    Node("N3", 3, "N3:8083", true)
  )

  def createRoutes(id: String): Array[Route] = {
    initNodes.filter(n => n.id != id)
      .map(n => Route(n.id, n.clz, n.other))
      .toArray
  }

  def init(): Unit = {
    staticRoutes = createRoutes("")

    for (node <- initNodes) {
      node.routes = createRoutes(node.id)
    }
  }

  def find(id: String): Node = {
    for (l <- initNodes) {
      if (l.id.equalsIgnoreCase(id)) return l
    }

    return null
  }

  def print(): Unit = {
    println("------------------route-------------------------")
    for (l <- initNodes) {
      l.printRoutes()
    }
  }

  def printData(): Unit = {
    println("-----------------data--------------------------")
    for (l <- initNodes) {
      l.printData()
    }
  }

  def nextNewNode(): Node = {
    val id = "N" + (initNodes.length + 1)
    val other = id + ":8080"
    var clz = Random.nextInt(1000) % (allClz.length)
    clz += 1

    val n = Node(id, clz, other, false)
    n.routes = staticRoutes
    initNodes += n
    n
  }

  def newNode(): Unit = {
    val newNode = nextNewNode()
    newNode.requestAndRefreshRoute()
    newNode.requestAddNode()

    newNode.alive = true
  }

  def randomNode(): Node = {
    val i = Random.nextInt(initNodes.length)
    val n = Linked.find(initNodes(i).id)
    n
  }

  def randomOffline(): Node = {
    val n = randomNode()
    n.offline()
    n
  }

  def cmd(): Unit = {
    println("1. print route")
    println("2. print data")
    println("3. new node")
    println("4. offline ")
    println("5. online")
    println("6. publish data")
    println("9. show menu")
    println("0. quit")
  }

  def getKey(f: Int => Boolean): Int = {
    while (true) {
      try {
        val k = StdIn.readInt()
        if (f(k)) {
          return k
        } else {
          println("invalid, try again")
        }
      } catch {
        case _: NumberFormatException =>
          println("that's not a number, try again")
      }
    }
    0
  }

  def mainMenu(): Int = {
    while (true) {
      try {
        val k = StdIn.readInt()
        if (k == 0) {
          return 0
        }

        k match {
          case 1 => print()
          case 2 => printData()
          case 3 => newNode()
          case 4 => {
            cmdGetNode().offline()
          }
          case 5 => {
            cmdGetNode().online()
          }
          case 6 => {
            cmdGetNode().publishData()
          }
          case 9 => cmd()
          case _ => println("invalid, try again")
        }
      } catch {
        case _: NumberFormatException =>
          println("that's not a number, try again")
      }
    }

    0
  }

  def cmdGetNode(): Node = {
    println("please input node id: ")
    val n = getKey(i => (i <= initNodes.length && i > 0))
    find("N" + n)
  }

  def main(args: Array[String]): Unit = {
    init()
    cmd()
    mainMenu()
  }
}
