package cn.edu.neu.chiewen.cknn.demo

import akka.actor.{Actor, ActorRef, Props}
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory

/**
 * Created by chiewen on 9/8/14.
 */
class OrderKVoronoiActor extends Actor {

  private var listSender: Option[ActorRef] = None

  private var clip: List[Array[(Double, Double)]] = Nil

  def receive = {
    case StartCalcOrderK(knn, list) =>
      listSender = Some(sender)
      for (p <- knn) {
        context.actorOf(Props[SingleVoronoiActor]) ! ProcessVoronoiMsg(p :: list)
      }
    case VoronoiProcessedMsg(result) =>
      clip ::= result
      if (clip.size == DemoData.k) listSender.map(_ ! clip)
  }
}

case class StartCalcOrderK(knn: List[NeighboredSiteMemory], list: List[NeighboredSiteMemory])