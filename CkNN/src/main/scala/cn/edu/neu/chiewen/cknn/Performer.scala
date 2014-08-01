package cn.edu.neu.chiewen.cknn

import cn.edu.neu.chiewen.cknn.algorithms.{EvaluateClient, EvaluateTracer, Util}
import cn.edu.neu.chiewen.cknn.server.ServerMemory
import cn.edu.neu.chiewen.cknn.settings.{PerformSettings, Settings}
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory

import scala.collection.JavaConversions.asScalaBuffer

object Performer {
  val conf = Settings.applicationContext.getBean("performSettings").asInstanceOf[PerformSettings]

  def run {
    for (s <- conf.servers) {
      implicit val server = s
      println("\n===========================" + server.describe + "===========================")
      for (t <- conf.trajectories.get) {
        s.addResultChildren(t)
        implicit val trajectory = t
        println(t.factory.describe)
        for (c <- conf.clients) {
          val e = new EvaluateClient(c)
          t.addResultChildren(e)
          new EvaluateTracer(e).traceTrajectory
        }
      }
      s.showResult("st")
      s.showResult("ct")
      s.showResult("tt")
      s.showResult("r")
      s.showResult("c")
      s.showResult("t")
      s.showResult("km")
    }
  }

  def illustrate {
    val server = Settings.applicationContext.getBean("serverMemory").asInstanceOf[ServerMemory]
    val conf = Settings.applicationContext.getBean("performSettings").asInstanceOf[PerformSettings]

    Util.illustrate(server.tree, server.tree.entries.asInstanceOf[List[NeighboredSiteMemory]], conf.trajectories.get)
  }

}
