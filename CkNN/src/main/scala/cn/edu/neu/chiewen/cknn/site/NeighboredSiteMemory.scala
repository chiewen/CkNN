package cn.edu.neu.chiewen.cknn.site

import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.rtree.core.Mbr

class NeighboredSiteMemory(
                            val id: Int,
                            val position: (Double, Double),
                            var neighbors: Set[NeighboredSiteMemory] = Set[NeighboredSiteMemory]())
  extends Mbr(List((position._1, position._1), (position._2, position._2))) with NeighboredSite {
  def getNeighbors(implicit server: Server = null) = neighbors.toList

  def getNeighborSize = neighbors.size
}