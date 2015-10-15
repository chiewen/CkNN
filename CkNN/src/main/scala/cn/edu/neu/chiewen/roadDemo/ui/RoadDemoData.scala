package cn.edu.neu.chiewen.roadDemo.ui

import java.awt.Point
import java.io.RandomAccessFile

import cn.edu.neu.chiewen.roadDemo.road.algorithm.Dijkstra
import cn.edu.neu.chiewen.roadDemo.road.{Node, Position, Road, RoadPosition}

import scala.swing.Publisher
import scala.swing.event.Event


case object RepaintEvent extends Event

case object TrajectoryFinishBuildingEvent extends Event

/**
 * Created by chiewen on 2015/9/21 20:44.
 */
object RoadDemoData extends Publisher {
  val dataFile = """CkNN/src/main/resources/roadNetwork.co"""
  var nodes = Set[Node]()
  var roads = List[Road]()
  var k = 5

  def toggleSite(point: Point) {
    findNodeByPosition(point) match {
      case Some(n) => n.isSite = !n.isSite
      case None =>
    }
  }

  def findNodeByPosition(point: Point): Option[Node] = {
    val candidates = nodes.filter(n => Math.abs(n.x - point.getX) < 5 && Math.abs(n.y - point.getY) < 5)
    if (candidates.isEmpty) None
    else Some(candidates.minBy(_.distanceTo(new Position(point.getX, point.getY))))
  }

  def removeLastRoad(): Unit = {
    if (roads.isEmpty) return

    val roadToDelete = roads.head

    val n1: Node = roadToDelete.points.head.asInstanceOf[Node]
    val n2: Node = roadToDelete.points.last.asInstanceOf[Node]

    if (n1.roads.size == 1) nodes -= n1
    else n1.roads = n1.roads.filterNot(r => r == roadToDelete)
    if (n2.roads.size == 1) nodes -= n2
    else n2.roads = n2.roads.filterNot(r => r == roadToDelete)

    roads = roads.tail
  }

  def writeRoadNetwork() {
    val data = new RandomAccessFile(dataFile, "rw")

    data.writeInt(nodes.size)
    for (n <- nodes) {
      data.writeDouble(n.x)
      data.writeDouble(n.y)
      data.writeLong(n.id)
      data.writeBoolean(n.isSite)
    }

    data.writeInt(roads.size)
    for (r <- roads) {
      data.writeInt(r.points.size)
      data.writeLong(r.points.head.asInstanceOf[Node].id)
      data.writeLong(r.points.last.asInstanceOf[Node].id)
      r.points.tail foreach { p => data.writeDouble(p.x); data.writeDouble(p.y) }
    }

    data.close()
  }

  def readRoadNetwork(): Unit = {
    reset()

    val data = new RandomAccessFile(dataFile, "rw")

    val nodeLength = data.readInt()
    for (i <- 1 to nodeLength) {
      nodes += new Node(data.readDouble(), data.readDouble(), data.readLong(), data.readBoolean())
    }

    val roadBuilder = Vector.newBuilder[Position]
    val roadLength = data.readInt()
    for (i <- 1 to roadLength) {
      val size = data.readInt()
      val id1 = data.readLong()
      val id2 = data.readLong()
      val node1: Node = nodes.find(_.id == id1) match {
        case Some(n) => n
        case None => println("null" + id1); null
      }
      val node2: Node = nodes.find(_.id == id2) match {
        case Some(n) => n
        case None => println("null" + id2); null
      }

      roadBuilder += node1
      for (i <- 1 to size - 2) {
        roadBuilder += new Position(data.readDouble() , data.readDouble() )
      }
      data.readDouble()
      data.readDouble()
      roadBuilder += node2
      roads ::= new Road(roadBuilder.result(): _*)
      roadBuilder.clear()
    }

    val maxId = nodes.maxBy(_.id).id

    cn.edu.neu.chiewen.roadDemo.road.setId(maxId + 1)

    prepareVoronoiInfo()

    publish(RepaintEvent)
  }

  def prepareVoronoiInfo(): Unit = {
    for (n <- nodes) Dijkstra.kNN(n)

    for (r <- roads) {
      val node1 = r.points.head.asInstanceOf[Node].nearestSites.head
      val node2 = r.points.last.asInstanceOf[Node].nearestSites.head
      if (node1 != null && node2 != null) {
        if (node1._2 != node2._2) {
          node1._2.neighborSites += node2._2
          node2._2.neighborSites += node1._2

          node1._2.voronoiRoads ::=(r, Some(RoadPosition(r, r.points.head.asInstanceOf[Node], (r.distance - node1._1 + node2._1) / 2)))
          node2._2.voronoiRoads ::=(r, Some(RoadPosition(r, r.points.last.asInstanceOf[Node], (r.distance - node2._1 + node1._1) / 2)))
        }
        else node1._2.voronoiRoads ::=(r, None)
      }
    }
  }

  def reset() {
    nodes = Set[Node]()
    roads = List[Road]()
  }
}
