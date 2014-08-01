package cn.edu.neu.chiewen.cknn.server

import javax.annotation.PostConstruct

import cn.edu.neu.chiewen.cknn.site.{NeighboredSite, Site}
import cn.edu.neu.chiewen.cknn.vtree.VTreeIO

import scala.beans.BeanProperty

class ServerDisk extends Server {
  @BeanProperty var dataFile = ""
  @BeanProperty var indexFile = ""

  private var tree: VTreeIO = null

  @PostConstruct private def getTree {
    tree = new VTreeIO(dataFile, indexFile)
  }

  def describe = "Disk based server"

  override def get(id: Long): NeighboredSite = tree.get(id)

  def getKNN(k: Int, position: (Double, Double)): (List[Site], Int) = tree.knn(k, position)
}