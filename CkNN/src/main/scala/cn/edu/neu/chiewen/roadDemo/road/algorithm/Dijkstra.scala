package cn.edu.neu.chiewen.roadDemo.road.algorithm

import cn.edu.neu.chiewen.roadDemo.road.Node

import scala.annotation.tailrec

/**
 * Created by chiewen on 2015/9/19 10:26.
 */
object Dijkstra {
  implicit val orderingNode = Ordering.by[(Double, Node), Double](_._1)

  def kNN(nodeRoot: Node, k: Int = 1) {
    var U: Vector[(Double, Node)] = for (n <- nodeRoot.neighbors) yield (nodeRoot.roadDistanceTo(n), n)
    var S: Set[Node] = Set.empty[Node]

    U +:=(0D, nodeRoot)

    @tailrec
    def execute() {
      if (U.nonEmpty) U.min match {
        case (length: Double, node: Node)
          if node.isSite && nodeRoot.nearestSites.size == k - 1 =>
          nodeRoot.nearestSites ::=(length, node)
        case nextNode: (Double, Node) =>
          if (nextNode._2.isSite) nodeRoot.nearestSites ::= nextNode
          for (n <- nextNode._2.neighbors if !S.contains(n)) U.indexWhere(_._2 == n) match {
            case -1 => U +:=(nextNode._1 + nextNode._2.roadDistanceTo(n), n)
            case i: Int => if (U(i)._1 > nextNode._1 + nextNode._2.roadDistanceTo(n))
              U = U.updated(i, (nextNode._1 + nextNode._2.roadDistanceTo(n), n))
          }
          U = U.updated(U.indexOf(nextNode), U.head).drop(1)
          S += nextNode._2
          execute()
      }
    }

    nodeRoot.nearestSites = List.empty[(Double, Node)]
    execute()
    nodeRoot.nearestSites = nodeRoot.nearestSites.reverse
  }
}
