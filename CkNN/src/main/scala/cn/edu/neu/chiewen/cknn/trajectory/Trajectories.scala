package cn.edu.neu.chiewen.cknn.trajectory

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import cn.edu.neu.chiewen.cknn.settings.{Settings, TrajectorySettings}

import scala.beans.BeanProperty
import scala.collection.JavaConversions.asScalaBuffer

class Trajectories {
  @BeanProperty var file = ""

  def get = {
    val f = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)))
    val ts = f.readObject().asInstanceOf[List[Trajectory]]
    f.close()
    ts
  }
}

object Trajectories {
  def write(file: String) {
    val f = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    f.writeObject(Settings.applicationContext.getBean("trajectorySettings").asInstanceOf[TrajectorySettings].factories.map(_.trajectory).toList)
    f.close()
  }
}
