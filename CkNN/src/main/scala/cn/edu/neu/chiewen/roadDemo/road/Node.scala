package cn.edu.neu.chiewen.roadDemo.road

/**
 * Created by Chiewen on 2015/9/15.
 */
class Node(x: Double, y: Double, val id: Long = getID,
           var isSite: Boolean = false, var roads: Vector[Road] = Vector.empty)
  extends Position(x, y) {
  var nearestSites: List[(Double, Node)] = List.empty
  var neighborSites: Set[Node] = Set.empty //valid when this.isSite
  var voronoiRoads: List[(Road, Option[RoadPosition])] = List.empty //valid when this.isSite

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        that.id == this.id &&
        that.x == this.x &&
        that.y == this.y 
    case _ => false
  }

  override def hashCode: Int =
    41 * (41 * (41 * id.toInt) + (x * 100).toInt) + (y * 100).toInt

  def asSite: Node = {
    isSite = true
    this
  }

  def neighbors = for (r <- roads) yield neighborByRoad(r)

  def neighborByRoad(r: Road): Node = {
    assert(r.terminals.contains(this))
    (r.terminals - this).head
  }

  def roadDistanceTo(n: Node): Double = {
    roads.find(_.terminals.contains(n)) match {
      case None => throw new Exception("road does not exist")
      case Some(r) => r.distance
    }
  }
}
