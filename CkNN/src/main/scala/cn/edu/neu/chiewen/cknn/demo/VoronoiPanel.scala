package cn.edu.neu.chiewen.cknn.demo

import java.awt.{Polygon, Color, geom}

import cn.edu.neu.chiewen.cknn.algorithms.Util
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._

/**
 * Created by chiewen on 7/8/14.
 */
class VoronoiPanel extends Panel {
  background = Color.white
  preferredSize = (VoronoiPanel.WIDTH, VoronoiPanel.HEIGHT)

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys)

  reactions += {
    case e: MousePressed =>
      DemoData.query = e.point
      requestFocusInWindow()
      path.reset()
      repaint
    case e: MouseMoved => {
      path.reset()
      if (DemoData.query != null) {
        moveTo(DemoData.query)
        lineTo(e.point)
      }
    }
    case KeyTyped(_, 'c', _, _) =>
      path.reset
      repaint
    case _: FocusLost => repaint()
  }

  def reset() {
    //draw order-1 Voronoi cells
    pVor = new geom.GeneralPath
    if (DemoData.voronoi != null) {
      for (p <- DemoData.voronoi) {
        pVor.moveTo(p.last._1, p.last._2)
        for (m <- p) pVor.lineTo(m._1, m._2)
      }
    }
    path.reset
    repaint
  }

  /* records the dragging */
  var path = new geom.GeneralPath
  var pVor = new geom.GeneralPath
  var pClip = new geom.GeneralPath

  def lineTo(p: Point) {
    path.lineTo(p.x, p.y)
    repaint()
  }

  def moveTo(p: Point) {
    path.moveTo(p.x, p.y)
    repaint()
  }

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)

    def fillCircle(i: NeighboredSiteMemory, e: Int = 0) {
      g.fillOval(i.position._1.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        i.position._2.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        VoronoiPanel.POINT_WIDTH + 2 * e, VoronoiPanel.POINT_WIDTH + 2 * e)
    }


    if (DemoData.rnn != null) {
      g.setColor(Color.cyan)
      var polygons: List[Polygon] = Nil
      for (c <- DemoData.clip) {
        val p = new Polygon()
        p.addPoint(c.last._1.toInt, c.last._2.toInt)
        for (i <- c)
          p.addPoint(i._1.toInt, i._2.toInt)
        polygons ::= p
      }
      val all = new Rectangle(0, 0, VoronoiPanel.WIDTH * 2, VoronoiPanel.HEIGHT * 2)
      g.setClip(all)
      polygons foreach (p => g.clip(p))
      g.fillRect(0, 0, VoronoiPanel.WIDTH * 2, VoronoiPanel.HEIGHT * 2)
      g.setClip(all)
    }


    g.setColor(Color.blue)
    if (DemoData.points != null) for (i <- DemoData.points) fillCircle(i)

    if (DemoData.rnn != null) {
      g.setColor(Color.red)
      for (i <- DemoData.rnn) fillCircle(i)
      val disR = Util.pointsDistance((DemoData.query.x, DemoData.query.y),
        DemoData.rnn((DemoData.k * DemoData.rho).toInt - 1).position)
      g.drawOval((DemoData.query.x - disR).toInt, (DemoData.query.y - disR).toInt, 2 * disR.toInt, 2 * disR.toInt)
      g.setColor(Color.green)
      for (i <- DemoData.knn) fillCircle(i)
      val disK = Util.pointsDistance((DemoData.query.x, DemoData.query.y), DemoData.rnn(DemoData.k - 1).position)
      g.drawOval((DemoData.query.x - disK).toInt, (DemoData.query.y - disK).toInt, 2 * disK.toInt, 2 * disK.toInt)
      g.setColor(Color.black)
      g.fillRect(DemoData.query.x - VoronoiPanel.POINT_WIDTH / 2,
        DemoData.query.y - VoronoiPanel.POINT_WIDTH / 2,
        VoronoiPanel.POINT_WIDTH, VoronoiPanel.POINT_WIDTH)

      g.setColor(Color.green)
      val e = 1
      for (i <- DemoData.ins) g.drawOval(i.position._1.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        i.position._2.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        VoronoiPanel.POINT_WIDTH + 2 * e, VoronoiPanel.POINT_WIDTH + 2 * e)
    }

    g.setColor(Color.black)
    g.draw(path)
    g.setColor(Color.lightGray)
    g.draw(pVor)
  }
}

object VoronoiPanel {
  val HEIGHT: Int = 300
  val WIDTH: Int = 300
  val POINT_WIDTH: Int = 6
}