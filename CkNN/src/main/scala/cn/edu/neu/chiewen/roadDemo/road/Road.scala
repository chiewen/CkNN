package cn.edu.neu.chiewen.roadDemo.road


/**
 * Created by Chiewen on 2015/9/15.
 */
class Road(ps: Position*) {
  lazy val terminals: Set[Node] = Set(points.head.asInstanceOf[Node], points.last.asInstanceOf[Node])
  lazy val distance = points.sliding(2).map { case Seq(x, y) => x.distanceTo(y) }.sum
  val points: Vector[Position] = Vector.empty ++ ps

  points.head.asInstanceOf[Node].roads +:= this
  points.last.asInstanceOf[Node].roads +:= this

  def getPoints(nodeFrom: Node): Vector[Position] = {
    if (nodeFrom == points.head) points
    else if (nodeFrom == points.last) points.reverse
    else throw new Exception("terminal node does not belong to the road")
  }
}
