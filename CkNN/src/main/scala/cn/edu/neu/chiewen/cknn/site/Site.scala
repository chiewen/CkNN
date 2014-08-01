package cn.edu.neu.chiewen.cknn.site

trait Site {
  val id: Int
  val position: (Double, Double)
}

object Site {

  import scala.language.implicitConversions

  implicit def Site2DoubleTuple(n: Site) = n.position
}