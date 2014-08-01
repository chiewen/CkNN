package cn.edu.neu.chiewen.cknn.algorithms

import java.lang.Math.{pow, sqrt}
import cn.edu.neu.chiewen.cknn.{MatlabColor, MatlabFunctions}
import cn.edu.neu.chiewen.cknn.settings.{IllustrateSettings, Settings}
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory
import cn.edu.neu.chiewen.cknn.trajectory.Trajectory
import cn.edu.neu.chiewen.cknn.vtree.VTree
import cn.edu.neu.chiewen.rtree.core.Mbr

object Util {

  import scala.language.implicitConversions

  def pointsDistance(l: (Double, Double), r: (Double, Double)) = sqrt(pow(l._1 - r._1, 2) + pow(l._2 - r._2, 2))

  def pointsDistanceNS(l: (Double, Double), r: (Double, Double)) = pow(l._1 - r._1, 2) + pow(l._2 - r._2, 2)

  implicit def NeighboredSiteMem2DoubleTuple(p: NeighboredSiteMemory): (Double, Double) = (p.coordinates(0)._1, p.coordinates(1)._1)

  implicit def DoubleTuple2Mbr(p: (Double, Double)) = new Mbr(List((p._1, p._1), (p._2, p._2)))

  def printKnn(knn: List[NeighboredSiteMemory]) {
    for (p <- knn) print(p.id + " ")
    println()
  }

  def illustrate(tree: VTree, sites: List[NeighboredSiteMemory], trajectories: List[Trajectory]) {
    val conf = Settings.applicationContext.getBean("illustrateSettings").asInstanceOf[IllustrateSettings]
    if (conf.show) {
      val plotter = new MatlabFunctions
      if (conf.showVoronoi) plotter.drawVoronoi(sites)
      if (conf.showRTreeMbr) tree.getRoot.traverse(0) {
        plotter.drawRectangle
      }
      if (conf.showPoints) plotter.drawPoints(sites)
      if (conf.showTrajectory) for (t <- trajectories) plotter.drawLines(MatlabColor.nextColor, t)
      if (conf.showPointNames) sites foreach plotter.drawName
      plotter.hold
    }
  }
}