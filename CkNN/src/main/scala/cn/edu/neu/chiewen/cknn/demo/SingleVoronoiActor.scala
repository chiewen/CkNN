package cn.edu.neu.chiewen.cknn.demo

import akka.actor.Actor
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory

/**
 * Created by chiewen on 9/8/14.
 */
class SingleVoronoiActor extends Actor{
  def receive = {
    case ProcessVoronoiMsg(list) =>
      sender ! VoronoiProcessedMsg(DemoData.singleVoronoi(list))
    case _ => println("Error: message not recognized")
  }
}

case class ProcessVoronoiMsg(list: List[NeighboredSiteMemory])
case class VoronoiProcessedMsg(result: Array[(Double, Double)])