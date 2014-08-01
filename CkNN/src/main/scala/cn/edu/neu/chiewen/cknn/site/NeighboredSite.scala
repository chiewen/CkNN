package cn.edu.neu.chiewen.cknn.site

import cn.edu.neu.chiewen.cknn.server.Server

trait NeighboredSite extends Site {
  val mid: Long

  def getNeighbors(implicit server: Server): List[NeighboredSite]
}

