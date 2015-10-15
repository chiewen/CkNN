package cn.edu.neu.chiewen.roadDemo.moving

import akka.actor.ActorSystem
import cn.edu.neu.chiewen.roadDemo.road.Node
import cn.edu.neu.chiewen.roadDemo.ui.{RepaintEvent, RoadDemoData, TrajectoryFinishBuildingEvent}

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing.Publisher
import scala.swing.event.Event

case object ObjectMovesEvent extends Event

case object KnnRefreshEvent extends Event

/**
 * Created by Chiewen on 2015/9/29.
 */
object MovingController extends Publisher {
  var stepDistance: Double = 1
  var stepTime: Int = 50
  val stepTimeMax: Int = 200
  var obj: MovingObject = null
  var knn = List.empty[Node]
  var voronoiNeighbor = List.empty[Node]

  var dataKnn = Array[Array[String]]()
  var dataVoronoi = Array[Array[String]]()

  var trajectoryNodes: List[Node] = List.empty[Node]

  def resetTrajectory() {
    trajectoryNodes = List.empty[Node]
  }

  def buildTrajectory() {
    obj = new MovingObject(trajectoryNodes.reverse: _*)
    trajectoryNodes = List.empty[Node]
    publish(TrajectoryFinishBuildingEvent)
  }

  def addTrajectoryNode(node: Option[Node]) {
    node match {
      case Some(n) =>
        if (trajectoryNodes.isEmpty || trajectoryNodes.head.roads.exists(_.terminals.contains(n)))
          trajectoryNodes ::= n
      case None =>
    }
  }

  def move() {
    val actorSystem = ActorSystem()
    val scheduler = actorSystem.scheduler
    implicit val executor = actorSystem.dispatcher

    def _move(ob: MovingObject): Unit = {
      scheduler.scheduleOnce(stepTime milliseconds) {
        ob.advance(stepDistance)
        publish(ObjectMovesEvent)
        if (!ob.isFinished) {
          val newKnn = ob.kNN(RoadDemoData.k)
          if (newKnn != knn) {
            knn = newKnn
            voronoiNeighbor = MovingObject.neighbors(knn)

            dataKnn = Array[Array[String]]()
            dataVoronoi = Array[Array[String]]()

            var i = knn.size + 1
            for (n <- knn.reverse) { i -= 1; dataKnn +:= Array(i.toString, n.id.toString, n.x.toString, n.y.toString)}
            i = voronoiNeighbor.size + 1
            for (n <- voronoiNeighbor) { i -= 1; dataVoronoi +:= Array(i.toString, n.id.toString, n.x.toString, n.y.toString)}

            publish(KnnRefreshEvent)
          }
        }
        publish(RepaintEvent)
        if (!ob.isFinished) _move(ob)
      }
    }

    if (obj != null && !obj.isFinished)
      _move(obj)
  }

}
