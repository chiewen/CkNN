package cn.edu.neu.chiewen.cknn.algorithms

import cn.edu.neu.chiewen.cknn.client.{Client, ClientTracer}
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.trajectory.Trajectory

class EvaluateTracer(val c: Client)(implicit server: Server, trajectory: Trajectory) extends ClientTracer(c) {
  var t: Long = 0

  override def preTrace {
    t -= System.nanoTime()
  }

  override def postTrace {
    t += System.nanoTime()
    c.addResult("st", (c.st / 1000000.0).toString)
    c.addResult("ct", (c.ct / 1000000.0).toString)
    c.addResult("tt", (t / 1000000.0).toString)
    c.addResult("r", c.retrievalCount.toString)
    c.addResult("t", c.trafficCount.toString)
    c.addResult("km", c.knnMiss.toString)
    c.addResult("cr", c.correctness.toString)
    c.addResult("c", c.communicationCount.toString)
    println(c.describe + "\t" + "%10.3f".format(t / 1000000.0))
  }
}