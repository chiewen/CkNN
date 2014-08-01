package cn.edu.neu.chiewen.cknn.trajectory

trait TrajectoryBuilder {
  def next: (Double, Double)

  def describe: String
}