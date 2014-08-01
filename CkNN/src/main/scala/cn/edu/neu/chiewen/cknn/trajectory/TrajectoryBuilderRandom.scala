package cn.edu.neu.chiewen.cknn.trajectory

import cn.edu.neu.chiewen.cknn.settings.Settings.globalSetting

import scala.beans.BeanProperty
import scala.util.Random

class TrajectoryBuilderRandom
  extends TrajectoryBuilder with Serializable {
  @BeanProperty var step = 0.0
  @BeanProperty var angle = 0.0

  var lastPosition: (Double, Double) = null
  var lastAngle: Double = 0.0


  def init {
    lastPosition = (Random.nextDouble() * globalSetting.width,
      Random.nextDouble() * globalSetting.height);
    lastAngle = Random.nextDouble * 6.28;
  }

  def next = {
    if (lastPosition == null) init

    def isIn(ll: (Double, Double)): Boolean = {
      0 < ll._1 && globalSetting.height > ll._2 && globalSetting.width > ll._1 && 0 < ll._2
    }

    def newPos(newAngle: Double): (Double, Double) = {
      (lastPosition._1 + step * Math.sin(newAngle), lastPosition._2 + step * Math.cos(newAngle))
    }
    //    val newAngle = (Random.nextDouble - .5) * angle * .6 + lastAngle;
    val newAngle = (Random.nextDouble - .5) * angle + lastAngle;

    lastAngle = newAngle

    var l = newPos(newAngle)

    while (!isIn(l)) {
      lastAngle = Random.nextDouble * 6.28
      l = newPos(lastAngle)

    }
    lastPosition = l
    lastPosition
  }


  def describe = " step:" + step + " angle:" + angle
}
