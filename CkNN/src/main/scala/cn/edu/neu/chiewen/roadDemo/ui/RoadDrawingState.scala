package cn.edu.neu.chiewen.roadDemo.ui

import java.awt.Point

import cn.edu.neu.chiewen.roadDemo.road.{Node, Position, Road}

/**
 * Created by chiewen on 2015/9/22 21:10.
 */
object RoadDrawingState {

  var points: List[Position] = Nil

  def addPoint(point: Point) {
    if (points.isEmpty) addNode(point)
    else points ::= new Position(point.getX, point.getY)
  }

  private def addNode(point: Point): Unit = {
    RoadDemoData.findNodeByPosition(point) match {
      case Some(p) => points ::= p
      case None =>
        val node = new Node(point.getX, point.getY)
        points ::= node
        RoadDemoData.nodes += node
    }
  }

  def addRoad(point: Point): Unit = {
    addNode(point)
    val road: Road = new Road(points: _*)
    RoadDemoData.roads ::= road

    println("nodes:" + RoadDemoData.nodes.size + " roads:" + RoadDemoData.roads.size + " current added:" + road.points.size)
    points = Nil
  }


}
