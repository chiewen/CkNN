package cn.edu.neu.chiewen.cknn

import Plotter.MatlabPlotter
import cn.edu.neu.chiewen.cknn.settings.Settings.globalSetting
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory
import cn.edu.neu.chiewen.cknn.trajectory.Trajectory
import cn.edu.neu.chiewen.rtree.core.Mbr
import com.mathworks.toolbox.javabuilder.{MWArray, MWClassID, MWComplexity, MWNumericArray}

class MatlabFunctions {
  val thePlot = new MatlabPlotter
  thePlot.holdon()

  def holdon() {
    thePlot.holdon()
  }

  def holdoff() {
    thePlot.holdoff()
  }

  def drawLines(color: MatlabColor.Color, trj: Trajectory) {
    val dims = Array(1, trj.positions.length)
    val x = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)
    val y = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)

    trj.positions.zipWithIndex.foreach { p =>
      x.set(p._2 + 1, p._1._1)
      y.set(p._2 + 1, p._1._2)
    }

    try {
      val r = thePlot.drawplot(x, y, getColor(color))
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
      MWArray.disposeArray(y)
    }
  }

  def drawName(p: NeighboredSiteMemory) {
    drawName(p.coordinates(0)._1, p.coordinates(1)._1, p.id.toString)
  }

  def drawName(xx: Double, yy: Double, n: String) {
    val dims = Array(1, 1)
    val x = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)
    val y = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)

    x.set(1, xx)
    y.set(1, yy)

    try {
      val r = thePlot.name(x, y, n)
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
      MWArray.disposeArray(y)
    }
  }

  def drawPoints(p: List[NeighboredSiteMemory]) {
    val dims = Array(1, p.length)
    val x = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)
    val y = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)
    val s = MWNumericArray.newInstance(Array(1, 1),
      MWClassID.DOUBLE, MWComplexity.REAL)
    val c = MWNumericArray.newInstance(Array(1, 3),
      MWClassID.DOUBLE, MWComplexity.REAL)

    s.set(1, globalSetting.pointRadius * 2)

    //see matlab help "scatter" for colors 
    c.set(1, 1)
    c.set(2, 0)
    c.set(3, 0)

    p.zipWithIndex.foreach { p =>
      x.set(p._2 + 1, p._1.coordinates(0)._1 + globalSetting.pointRadius)
      y.set(p._2 + 1, p._1.coordinates(1)._1 + globalSetting.pointRadius)
    }

    try {
      val r = thePlot.drawPoints(x, y, s, c)
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
      MWArray.disposeArray(y)
    }
  }

  def drawVoronoi(p: List[NeighboredSiteMemory]) {
    val dims = Array(1, p.length)
    val x = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)
    val y = MWNumericArray.newInstance(dims,
      MWClassID.DOUBLE, MWComplexity.REAL)

    p.zipWithIndex.foreach { p =>
      x.set(p._2 + 1, p._1.coordinates(0)._1 + globalSetting.pointRadius)
      y.set(p._2 + 1, p._1.coordinates(1)._1 + globalSetting.pointRadius)
    }

    try {
      val r = thePlot.drawVoronoi(x, y)
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
      MWArray.disposeArray(y)
    }
  }

  def drawRectangle(mbr: Mbr, color: Int) {
    val c = color match {
      case 1 => MatlabColor.Red
      case 2 => MatlabColor.Green
      case 3 => MatlabColor.Cyan
      case 4 => MatlabColor.Blue
      case 5 => MatlabColor.Magenta
      case 6 => MatlabColor.Yellow
      case _ => MatlabColor.Black
    }
    if (mbr.coordinates.length > 1 && mbr.coordinates(0)._2 - mbr.coordinates(0)._1 > 0)
      drawRectangle(mbr.coordinates(0)._1, mbr.coordinates(1)._1,
        mbr.coordinates(0)._2 - mbr.coordinates(0)._1,
        mbr.coordinates(1)._2 - mbr.coordinates(1)._1,
        c)
  }

  def drawRectangle(x: Double, y: Double, w: Double, h: Double, color: MatlabColor.Color) {

    val a = MWNumericArray.newInstance(Array(1, 4),
      MWClassID.DOUBLE, MWComplexity.REAL)

    a.set(1, x)
    a.set(2, y)
    a.set(3, w)
    a.set(4, h)

    try {
      thePlot.drawRectangle(a, getColor(color))
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
      MWArray.disposeArray(y)
    }
  }

  def hold() {
    try {
      thePlot.waitForFigures()
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      if (thePlot != null)
        thePlot.dispose()
    }
  }

  override def finalize() {
    if (thePlot != null)
      thePlot.dispose()
  }

  private def getColor(color: MatlabColor.Color): com.mathworks.toolbox.javabuilder.MWNumericArray = {

    val c = MWNumericArray.newInstance(Array(1, 3),
      MWClassID.DOUBLE, MWComplexity.REAL)
    import cn.edu.neu.chiewen.cknn.MatlabColor._
    color match {
      case Yellow => c.set(1, 1); c.set(2, 1); c.set(3, 0);
      case Green => c.set(1, 0); c.set(2, 1); c.set(3, 0);
      case Cyan => c.set(1, 0); c.set(2, 1); c.set(3, 1);
      case Red => c.set(1, 1); c.set(2, 0); c.set(3, 0);
      case Magenta => c.set(1, 1); c.set(2, 0); c.set(3, 1);
      case Blue => c.set(1, 0); c.set(2, 0); c.set(3, 1);
      case _ => c.set(1, 0); c.set(2, 0); c.set(3, 0);
    }
    c
  }
}

object MatlabColor extends Enumeration {
  type Color = Value
  val Yellow, Magenta, Cyan, Red, Green, Blue, Black = Value

  private var c = 4

  def nextColor: Color = {
    c = (c + 1) % 6
    c match {
      case 1 => Yellow
      case 2 => Magenta
      case 3 => Cyan
      case 4 => Red
      case 5 => Green
      case 6 => Blue
      case _ => Black
    }
  }
}
