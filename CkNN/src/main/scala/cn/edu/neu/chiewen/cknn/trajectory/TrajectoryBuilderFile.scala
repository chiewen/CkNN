package cn.edu.neu.chiewen.cknn.trajectory

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class TrajectoryBuilderFile
  extends TrajectoryBuilder with Serializable {
  @BeanProperty var filePath = "" //"/Users/chiewen/Research/data/trajectory/schoolbuses/Buses.txt"

  lazy val coordinates = {
    val ab = new ArrayBuffer[(Double, Double)]
    (Source.fromFile(filePath)("ISO-8859-1").getLines).take(320).foreach(s => {
      val a = s.split(";")
      println(s)
      val c = ((a(6).toDouble, a(7).toDouble))
      ab += c
      println(c)
    })
    ab.toArray
  }

  var pos = 0;

  def next = {
    pos += 1
    coordinates(pos)
  }

  def describe = " file:" + filePath
}
