package cn.edu.neu.chiewen.cknn.demo

import java.awt.{Color, Polygon, geom}

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

  var pVor = new geom.GeneralPath
  var pCompare = new geom.GeneralPath
  var isCalculating = false
  var needRefresh = false

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys, DemoData)

  reactions += {
    case e: MousePressed =>
      DemoData.query = e.point
      requestFocusInWindow()
      repaint()
    case e: MouseDragged =>
      DemoData.query = e.point
      repaint()
    case KeyTyped(_, 'r', _, _) =>
      DemoData.refresh()
      repaint()
    case NotNeedRefreshEvent =>
      needRefresh = false
      repaint()
    case NeedRefreshEvent =>
      needRefresh = true
      repaint()
    case BeginCalculatingEvent =>
      needRefresh = false
      isCalculating = true
      repaint()
    case DataFinishedEvent =>
      isCalculating = false
      repaint()
    case _: FocusLost => repaint()
  }

  def reset() {
    //order-1 Voronoi cells
    pVor.reset()
    if (DemoData.voronoi != null) {
      for (p <- DemoData.voronoi) {
        pVor.moveTo(p.last._1, p.last._2)
        for (m <- p) pVor.lineTo(m._1, m._2)
      }
    }
    repaint()
  }

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    pCompare.reset()
    val all = new Rectangle(0, 0, VoronoiPanel.WIDTH * 2, VoronoiPanel.HEIGHT * 2)

    def fillCircle(i: NeighboredSiteMemory, e: Int = 0) {
      g.fillOval(i.position._1.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        i.position._2.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
        VoronoiPanel.POINT_WIDTH + 2 * e, VoronoiPanel.POINT_WIDTH + 2 * e)
    }

    // order-k Voronoi cells
    if (DemoData.clip != null && DemoData.clip.size > 0) {
      if (needRefresh)
        g.setColor(new Color(255, 0, 0, 155))
      else
        g.setColor(Color.cyan)
      var polygons: List[Polygon] = Nil
      for (c <- DemoData.clip) {
        val p = new Polygon()
        for (i <- c)
          p.addPoint(i._1.toInt, i._2.toInt)
        polygons ::= p
      }
      g.setClip(all)
      polygons foreach (p => g.clip(p))
      g.fillRect(0, 0, VoronoiPanel.WIDTH * 2, VoronoiPanel.HEIGHT * 2)
      g.setClip(all)
    }

    //the objects
    g.setColor(Color.blue)
    if (DemoData.points != null) for (i <- DemoData.points) fillCircle(i)

    //Voronoi cells
    g.setColor(Color.lightGray)
    g.draw(pVor)

    if (DemoData.rnn != null) {
      //the compared pair
      val far = DemoData.max(DemoData.knn)
      val near = DemoData.min(DemoData.ins)
      g.setColor(Color.green)
      g.drawOval((far._1 - 8).toInt, (far._2 - 8).toInt, 16, 16)
      g.setColor(Color.red)
      g.drawOval((near._1 - 8).toInt, (near._2 - 8).toInt, 16, 16)

      val longEdge = Util.pointsDistance(far, near)
      val middle = ((far._1 + near._1) / 2, (far._2 + near._2) / 2)
      val p1 = (middle._1 + 10000 * (far._2 - near._2) / longEdge, middle._2 + 10000 * (near._1 - far._1) / longEdge)
      val p2 = (middle._1 - 10000 * (far._2 - near._2) / longEdge, middle._2 - 10000 * (near._1 - far._1) / longEdge)
      pCompare.moveTo(p1._1, p1._2)
      pCompare.lineTo(p2._1, p2._2)

      g.setColor(Color.red)
      val disR = Util.pointsDistance((DemoData.query.x, DemoData.query.y),
        DemoData.min(DemoData.ins).position)
      g.drawOval((DemoData.query.x - disR).toInt, (DemoData.query.y - disR).toInt, 2 * disR.toInt, 2 * disR.toInt)

      //ins
      for (i <- DemoData.ins) fillCircle(i)

      //knn
      g.setColor(Color.green)
      for (i <- DemoData.knn) fillCircle(i)
      val disK = Util.pointsDistance((DemoData.query.x, DemoData.query.y), DemoData.max(DemoData.knn).position)
      g.drawOval((DemoData.query.x - disK).toInt, (DemoData.query.y - disK).toInt, 2 * disK.toInt, 2 * disK.toInt)

      //query object
      g.setColor(Color.black)
      g.fillRect(DemoData.query.x - VoronoiPanel.POINT_WIDTH / 2,
        DemoData.query.y - VoronoiPanel.POINT_WIDTH / 2,
        VoronoiPanel.POINT_WIDTH, VoronoiPanel.POINT_WIDTH)

      //enlarged rnn set
      g.setColor(Color.green.darker())
      val e = 1
      //if ((DemoData.rho * DemoData.k).toInt > DemoData.k)
        for (i <- DemoData.rnn)//.filterNot(p => DemoData.knn.exists(e => e.id == p.id)))
          g.drawRect(i.position._1.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
            i.position._2.toInt - VoronoiPanel.POINT_WIDTH / 2 - e,
            VoronoiPanel.POINT_WIDTH + 2 * e, VoronoiPanel.POINT_WIDTH + 2 * e)
    }

    //the division line
    if (needRefresh) g.setColor(Color.red) else g.setColor(Color.green)
    g.draw(pCompare)

    g.setColor(Color.black)
    if (isCalculating && !needRefresh)
      g.drawString("Calculating Order-" + DemoData.k + " Voronoi cell...", 10, size.height - 10)
    if (needRefresh)
      g.drawString("The kNN set is invalid. Press 'r' to refresh.", 10, size.height - 10)
  }
}

object VoronoiPanel {
  val HEIGHT: Int = 300
  val WIDTH: Int = 300
  val POINT_WIDTH: Int = 6
}