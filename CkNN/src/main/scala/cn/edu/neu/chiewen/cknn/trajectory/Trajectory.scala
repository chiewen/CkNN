package cn.edu.neu.chiewen.cknn.trajectory

import cn.edu.neu.chiewen.cknn.ResultKeeper

class Trajectory(val positions: List[(Double, Double)], val factory: TrajectoryFactory) extends Serializable with ResultKeeper {
  def nowAt = positions.head

  def isEnd = positions.isEmpty

  def nextPosition = new Trajectory(positions.drop(1), factory)

  def showResult(s: String) {
    print("\t")
    for (i <- children) i.showResult(s)
    println
  }
}