package cn.edu.neu.chiewen.roadDemo.road

import scala.math.{pow, sqrt}

/**
 * Created by Chiewen on 2015/9/15.
 */
case class Position(x: Double, y: Double) {

  def distanceTo(other: Position): Double = sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))
}
