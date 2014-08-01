package cn.edu.neu.chiewen.cknn.trajectory

import scala.beans.BeanProperty

class TrajectoryFactory extends Serializable {
  @BeanProperty var segments: Int = 0
  @BeanProperty var builder: TrajectoryBuilder = null

  def trajectory = new Trajectory((1 to segments).map(_ => builder.next).toList, this)

  def describe = "\nTrajectoryFactory segments:" + segments + builder.describe
}
