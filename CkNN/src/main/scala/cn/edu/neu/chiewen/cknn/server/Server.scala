package cn.edu.neu.chiewen.cknn.server

import cn.edu.neu.chiewen.cknn.ResultKeeper
import cn.edu.neu.chiewen.cknn.site.{NeighboredSite, Site}

trait Server extends ResultKeeper {
  def get(id: Long): NeighboredSite = null

  def getKNN(k: Int, position: (Double, Double)): (List[Site], Int)

  def describe: String

  def showResult(s: String) {
    println("******  " + s + "  ******")
    for (i <- children.reverse) i.showResult(s)
    println
  }
}