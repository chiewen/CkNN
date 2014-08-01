package cn.edu.neu.chiewen.cknn.site

import cn.edu.neu.chiewen.cknn.settings.Settings.globalSetting

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random
import scala.util.Random.nextDouble

object SiteGenerator {
  //val file = "/Volumes/ChieDisk2T/Data/geocoderData/Canada.csv"
  val file = "/Users/chiewen/Downloads/CanData.csv"

  def getSites = {
    val lb = ListBuffer[NeighboredSiteMemory]()

    for (i <- 1 to globalSetting.pointNumber)
      lb.append(new NeighboredSiteMemory(i, (nextDouble * globalSetting.width, nextDouble * globalSetting.height)))

    lb.toList
  }

  def getSitesFromFile = {
    val lb = ListBuffer[NeighboredSiteMemory]()
    var i = 0
    (Source.fromFile(file)("ISO-8859-1").getLines).take(100000).foreach(s => {
      //val v = s.substring(0, s.indexOf(",", s.indexOf(",", s.indexOf(",") + 1) + 1))
      //println(s)
      val a = s.split("\",\"") //.map(a => a.replace('\"', ' ').trim())
      lb.append(new NeighboredSiteMemory(i, ((a(5).toDouble - 47) * 1000000 + Random.nextDouble,
        (a(6).toDouble + 53) * 1000000 + Random.nextDouble)))
      i = i + 1
    })

    lb.toList
  }
}