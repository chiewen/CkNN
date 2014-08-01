package cn.edu.neu.chiewen.cknn

import cn.edu.neu.chiewen.cknn.settings.{PerformSettings, Settings}
import cn.edu.neu.chiewen.cknn.trajectory.Trajectories

object TrajectoryGenerator extends App {
  val conf = Settings.applicationContext.getBean("performSettings").asInstanceOf[PerformSettings]

  Trajectories.write(Settings.globalSetting.dir_traj + "trajectories.trj")
  java.awt.Toolkit.getDefaultToolkit().beep();
}