package cn.edu.neu.chiewen.cknn.site

import cn.edu.neu.chiewen.cknn.server.Server

class NeighboredSiteDisk(val mid: Long, val id: Int, val position: (Double, Double)) extends NeighboredSite {
  def getNeighbors(implicit server: Server) = server.get(mid).getNeighbors

  override def hashCode = (41 * (41 + mid)).toInt

  override def equals(o: Any) = o match {
    case that: NeighboredSiteDisk => NeighboredSiteDisk.this.mid == that.mid
    case _ => false
  }
}
