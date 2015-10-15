package cn.edu.neu.chiewen.roadDemo.ui

import java.awt.{BasicStroke, Color, geom}
import javax.swing.{BorderFactory, ImageIcon, SwingUtilities}

import cn.edu.neu.chiewen.roadDemo.moving.MovingController
import cn.edu.neu.chiewen.roadDemo.road.{Node, Position, Road}
import cn.edu.neu.chiewen.roadDemo.ui.RoadVoronoiPanel._

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._

/**
 * Created by chiewen on 7/8/14.
 */
class RoadVoronoiPanel extends Panel with Publisher {
  background = Color.white
  border = BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED)

  preferredSize = (WIDTH, HEIGHT)

  val backImage = new ImageIcon( """CkNN/src/main/resources/shenyang.png""").getImage
  var pVor = new geom.GeneralPath
  var pRoad = new geom.GeneralPath
  var actionState = ActionState.Demo
  var auto = false

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys, RoadDemoData, MovingController)

  reactions += {
    case e: MouseClicked =>
      requestFocusInWindow()
      actionState match {
        case ActionState.Add =>
          if (SwingUtilities.isRightMouseButton(e.peer)) RoadDrawingState.addRoad(e.point)
          else RoadDrawingState.addPoint(e.point)
        case ActionState.Site => RoadDemoData.toggleSite(e.point)
        case ActionState.Trajectory => MovingController.addTrajectoryNode(RoadDemoData.findNodeByPosition(e.point))
        case ActionState.Demo =>
      }
      repaint()
    case e: MouseDragged =>
      repaint()
    case KeyTyped(_, 'r', _, _) if actionState == ActionState.Add =>
      RoadDemoData.removeLastRoad()
      repaint()
    case KeyTyped(_, 'r', _, _) if actionState == ActionState.Demo =>
      MovingController.obj.renew()
      MovingController.move()
    case KeyTyped(_, 's', _, _) =>
      actionState = ActionState.Site
      publish(ActionStateChangeEvent)
    case KeyTyped(_, 'd', _, _) =>
      actionState = ActionState.Demo
      publish(ActionStateChangeEvent)
    case KeyTyped(_, 'a', _, _) =>
      actionState = ActionState.Add
      publish(ActionStateChangeEvent)
    case KeyTyped(_, 't', _, _) =>
      actionState = ActionState.Trajectory
      publish(ActionStateChangeEvent)
      MovingController.resetTrajectory()
    case KeyTyped(_, 'f', _, _) if actionState == ActionState.Trajectory =>
      MovingController.buildTrajectory()
      MovingController.move()
    case TrajectoryFinishBuildingEvent =>
      actionState = ActionState.Demo
      publish(ActionStateChangeEvent)
      repaint()
    case RepaintEvent =>
      repaint()
    case _: FocusLost => repaint()
  }


  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    g.drawImage(backImage, 0, 0, this.size.width, this.size.height, null)

    def fillCircle(i: Position, e: Int = 0) {
      g.fillOval(i.x.toInt - POINT_WIDTH / 2 - e,
        i.y.toInt - POINT_WIDTH / 2 - e,
        POINT_WIDTH + 2 * e, POINT_WIDTH + 2 * e)
    }

    def drawCircle(i: Position, e: Int = 0) {
      g.drawOval(i.x.toInt - POINT_WIDTH / 2 - e,
        i.y.toInt - POINT_WIDTH / 2 - e,
        POINT_WIDTH + 2 * e, POINT_WIDTH + 2 * e)
    }

    def drawRoad(r: Road): Unit = {
      pRoad.reset()
      pRoad.moveTo(r.points.head.x, r.points.head.y)
      r.points.foreach(p => pRoad.lineTo(p.x, p.y))
      g.draw(pRoad)
    }

    if (PanelDrawingState.showRoad) {
      for (r <- RoadDemoData.roads) drawRoad(r)
    }

    if (RoadDrawingState.points.nonEmpty) {
      pRoad.reset()
      pRoad.moveTo(RoadDrawingState.points.head.x, RoadDrawingState.points.head.y)
      for (p <- RoadDrawingState.points) pRoad.lineTo(p.x, p.y)
      g.draw(pRoad)
    }

    if (PanelDrawingState.showNode) {
      RoadDemoData.nodes foreach {
        fillCircle(_)
      }
    }

    g.setColor(Color.orange)
    RoadDemoData.nodes.filter(_.isSite) foreach {
      fillCircle(_, 1)
    }

    g.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

    def drawVoronoi(n: Node): Unit = {
      for (v <- n.voronoiRoads) v._2 match {
        case Some(p) =>
          pRoad.reset()
          pRoad.moveTo(p.node.x, p.node.y)
          val ps = v._1.getPoints(p.node)
          ps.slice(1, p.pointNum + 1).foreach(s => pRoad.lineTo(s.x, s.y))
          pRoad.lineTo(p.x, p.y)
          g.draw(pRoad)
        case None => drawRoad(v._1)
      }
      fillCircle(n, 2)
      val c = g.getColor
      g.setColor(Color.black)
      drawCircle(n, 3)
      g.setColor(c)
    }

    if (PanelDrawingState.showKnn) {
      g.setColor(Color.green)
      if (MovingController.knn.nonEmpty)
        for (n <- MovingController.knn) drawVoronoi(n)
    }

    if (PanelDrawingState.showNeighbors) {
      g.setColor(Color.cyan)
      if (MovingController.voronoiNeighbor.nonEmpty)
        for (n <- MovingController.voronoiNeighbor) drawVoronoi(n)
    }

    g.setColor(Color.red)
    if (MovingController.obj != null) fillCircle(MovingController.obj.currentPosition, 3)
    if (MovingController.trajectoryNodes.size > 1) MovingController.trajectoryNodes.sliding(2).foreach { s =>
      s.head.roads.find(_.terminals.contains(s(1))) match {
        case Some(r) => drawRoad(r)
        case None => throw new Exception("no such road")
      }
    }
  }

  object ActionState extends Enumeration {
    val Add, Site, Demo, Trajectory = Value
  }

}

object RoadVoronoiPanel {
  val HEIGHT: Int = 900
  val WIDTH: Int = 1500
  val POINT_WIDTH: Int = 6
}

case object ActionStateChangeEvent extends Event