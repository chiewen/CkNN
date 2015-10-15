package cn.edu.neu.chiewen.roadDemo.moving

import cn.edu.neu.chiewen.roadDemo.road.algorithm.Dijkstra
import cn.edu.neu.chiewen.roadDemo.road.{Node, Position, Road}

import scala.annotation.tailrec

/**
 * Created by Chiewen on 2015/9/28.
 */
class MovingObject(val trajectory: Node*) {
  var nodeIterator = trajectory.iterator
  var node = nodeIterator.next()
  var pointNum: Int = 0
  var distanceFromPoint: Double = 0D

  @tailrec
  final def advance(distance: Double): Unit =
    advanceInRoad(currentRoad.getPoints(node), distance + distanceFromPoint) match {
      case left if left == 0D =>
      case left => if (nodeIterator.hasNext) advance(left)
    }

  def currentPosition: Position = {
    if (isFinished)
      node
    else {
      val a = currentRoad.getPoints(node)(pointNum)
      val b = currentRoad.getPoints(node)(pointNum + 1)
      val ratio = distanceFromPoint / a.distanceTo(b)
      new Position(a.x + ratio * (b.x - a.x), a.y + ratio * (b.y - a.y))
    }
  }

  def isFinished: Boolean = !nodeIterator.hasNext

  def renew() {
    nodeIterator = trajectory.iterator
    node = nodeIterator.next()
    pointNum = 0
    distanceFromPoint = 0D
  }

  def kNN(k: Int): List[Node] = {
    val n2 = trajectory(trajectory.indexOf(node) + 1)
    ensureKnnCalculated(k, node)
    ensureKnnCalculated(k, n2)

    val vb = Vector.newBuilder[(Double, Node)]

    val distanceFromNode: Double = distanceFromPoint +
      (if (pointNum > 0) currentRoad.getPoints(node).take(pointNum + 1).sliding(2).map(s => s.head.distanceTo(s(1))).sum else 0D)

    vb ++= node.nearestSites.map(r => (r._1 + distanceFromNode, r._2))

    val distanceToN2 = currentRoad.distance - distanceFromNode
    vb ++= n2.nearestSites.map(r => (r._1 + distanceToN2, r._2))

    var result = List.empty[Node]
    vb.result().sortBy(_._1).takeWhile { t =>
      if (result.size < k) {
        if (!result.contains(t._2)) result ::= t._2
        true
      }
      else false
    }
    result.reverse
  }

  def currentRoad: Road = node.roads.find(_.terminals.contains(
    trajectory(trajectory.indexOf(node) + 1))).get

  def ensureKnnCalculated(k: Int, n: Node): Unit = {
    if (n.nearestSites.size < k) Dijkstra.kNN(n, k)
  }

  @tailrec
  final private def advanceInRoad(points: Vector[Position], distance: Double): Double =
    points(pointNum).distanceTo(points(pointNum + 1)) match {
      case d if d > distance =>
        distanceFromPoint = distance
        0D
      case d if pointNum < points.size - 2 =>
        pointNum += 1
        advanceInRoad(points, distance - d)
      case d =>
        node = nodeIterator.next()
        pointNum = 0
        distanceFromPoint = 0D
        distance - d
    }
}

object MovingObject {
  def neighbors(knn: List[Node]): List[Node] = (for (n <- knn; b <- n.neighborSites) yield b).distinct.diff(knn)
}
