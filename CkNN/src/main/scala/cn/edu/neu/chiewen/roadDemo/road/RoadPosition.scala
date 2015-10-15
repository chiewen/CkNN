package cn.edu.neu.chiewen.roadDemo.road

/**
 * Created by chiewen on 2015/10/6 15:47.
 */
case class RoadPosition(var node: Node, var pointNum: Int, var x: Double, var y: Double)

object RoadPosition {
  def apply(road: Road, node: Node, length: Double) = {
    var pn = 0
    var sum = 0D
    val points = road.getPoints(node)

    //TODO make clear why using .toList
    points.sliding(2).toList.takeWhile { s =>
      sum += s.head.distanceTo(s(1))
      pn += 1
      if (sum >= length) false else true
    }

    val ratio = (length - sum + points(pn - 1).distanceTo(points(pn))) /
      math.sqrt(math.pow(points(pn).x - points(pn - 1).x, 2) + math.pow(points(pn).y - points(pn - 1).y, 2))

    new RoadPosition(node, pn - 1, points(pn - 1).x + ratio * (points(pn).x - points(pn - 1).x),
      points(pn - 1).y + ratio * (points(pn).y - points(pn - 1).y))
  }
}