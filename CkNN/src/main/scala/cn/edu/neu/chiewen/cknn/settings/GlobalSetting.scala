package cn.edu.neu.chiewen.cknn.settings

import scala.beans.BeanProperty

class GlobalSetting {
  @BeanProperty var width = 2000
  @BeanProperty var height = 2000

  @BeanProperty var pointNumber = 180
  @BeanProperty var pointRadius = 2

  @BeanProperty var dir_write = ""
  @BeanProperty var dir_traj = ""
}
