import sbt._

object Common extends Build {

  lazy val rtree = project in file("Rtree")

  lazy val cknn = (project in file("CkNN")).dependsOn(rtree)

  lazy val root = (project in file(".")). aggregate(rtree, cknn)
}


