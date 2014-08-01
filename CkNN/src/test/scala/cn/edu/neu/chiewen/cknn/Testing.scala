package cn.edu.neu.chiewen.cknn

import cn.edu.neu.chiewen.cknn.algorithms.cknn.CkNNClient
import cn.edu.neu.chiewen.cknn.algorithms.vsdiagram.VsClient
import cn.edu.neu.chiewen.cknn.settings.Settings.globalSetting
import cn.edu.neu.chiewen.cknn.settings.{PerformSettings, Settings}

import scala.collection.JavaConversions.asScalaBuffer
import scala.util.Random._

object Testing extends App {
  //	val t = new VTree(Nil)
  //	
  //	t.insert(new NeighboredSiteMemory(1, (2,2)))
  //	t.insert(new NeighboredSiteMemory(1, (3,3)))
  //	t.insert(new NeighboredSiteMemory(1, (4,4)))
  //	t.insert(new NeighboredSiteMemory(1, (5,5)))
  //	
  //	println(t.nnNode((3.6,3.6)).mbr)

  //  val n = 100000.0
  //  val k = 10.0
  //  val x = n * (Math.pow(1 - Math.pow(Math.sqrt(1 - Math.sqrt(k / n)) - Math.sqrt(Math.PI) / (2 * Math.sqrt(k * (n - k))), 2), 2)) - k
  //
  //  println(x)

  val conf = Settings.applicationContext.getBean("performSettings").asInstanceOf[PerformSettings]

  def testUpdate {
    val x = 20
    //for (server <- conf.servers) {
    val server = conf.servers(0)
    implicit val s = server
    val k = 50
    for (v <- 10 to 100 by 10) {

      val vc = new VsClient
      val cc = new CkNNClient

      vc.k = k
      vc.x = x
      cc.k = k
      val position = (nextDouble * globalSetting.width, nextDouble * globalSetting.height)

      vc.check(position)
      cc.check(position)

      for (i <- 1 to 30) {
        val n = nextInt(k)
        val d = vc.get(n)
        vc.delete(position, d)
        vc.insert(position, d)
      }

      var tv: Long = 0
      tv -= System.nanoTime
      for (i <- 1 to v) {
        val n = nextInt(k)
        val d = vc.get(n)
        vc.delete(position, d)
        vc.insert(position, d)
      }
      tv += System.nanoTime

      var cv: Long = 0
      cv -= System.nanoTime
      for (i <- 1 to v) {
        val n = nextInt(k)
        val d = cc.get(n)
        cc.delete(d, position)
        cc.deleteNeighbor(d.id)
        cc.insert(d, position)
      }
      cv += System.nanoTime

      println(v + "\t" + tv + "\t" + cv)
    }

    println("\n\n")
    java.awt.Toolkit.getDefaultToolkit().beep();
    //}
  }

  testUpdate

}