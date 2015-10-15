package cn.edu.neu.chiewen.cknn.client

import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.trajectory.Trajectory

class ClientTracer(val client: Client)(implicit server: Server, trajectory: Trajectory) {
  private var _trajectory: Trajectory = null

  def traceTrajectory() {
    preTrace()
    this._trajectory = trajectory
    while (!_trajectory.isEnd) {
      client.check(_trajectory.nowAt)
      _trajectory = _trajectory.nextPosition
    }
    postTrace()
  }

  def preTrace() {}

  def postTrace() {}
}
