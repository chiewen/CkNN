package cn.edu.neu.chiewen.cknn.settings

import java.util.ArrayList

import cn.edu.neu.chiewen.cknn.trajectory.TrajectoryFactory

import scala.beans.BeanProperty

class TrajectorySettings {
  @BeanProperty var factories: ArrayList[TrajectoryFactory] = null
}
