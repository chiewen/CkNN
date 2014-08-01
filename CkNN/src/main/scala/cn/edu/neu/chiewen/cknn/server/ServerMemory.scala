package cn.edu.neu.chiewen.cknn.server

import java.io.{FileInputStream, ObjectInputStream}

import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory
import cn.edu.neu.chiewen.cknn.vtree.VTree

import scala.beans.BeanProperty

class ServerMemory extends Server {
  @BeanProperty var file: String = ""

  lazy val tree: VTree = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val t = ois.readObject().asInstanceOf[VTree]
    ois.close()
    t
  }

  def describe = "Memory based server"

  def getKNN(k: Int, position: (Double, Double)): (List[NeighboredSiteMemory], Int) = tree.knn(k, position)
}
