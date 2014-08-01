package cn.edu.neu.chiewen.cknn.settings

import java.util.ArrayList

import cn.edu.neu.chiewen.cknn.client.Client
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.trajectory.Trajectories

import scala.beans.BeanProperty

class PerformSettings {
  @BeanProperty var servers: ArrayList[Server] = null
  @BeanProperty var clients: ArrayList[Client] = null
  @BeanProperty var trajectories: Trajectories = null
}

class IllustrateSettings {
  @BeanProperty var show = false
  @BeanProperty var showPoints = false
  @BeanProperty var showVoronoi = false
  @BeanProperty var showTrajectory = false
  @BeanProperty var showRTreeMbr = false
  @BeanProperty var showPointNames = false
}