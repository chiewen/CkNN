package cn.edu.neu.chiewen.cknn.demo

import Plotter.MatlabPlotter
import akka.actor.{ActorSystem, Props}
import akka.dispatch.ExecutionContexts._
import akka.pattern.ask
import akka.util.Timeout
import cn.edu.neu.chiewen.cknn.algorithms.Util._
import cn.edu.neu.chiewen.cknn.site.{NeighboredSiteMemory, SiteGenerator}
import cn.edu.neu.chiewen.cknn.vtree.VTree
import com.mathworks.toolbox.javabuilder.{MWArray, MWClassID, MWComplexity, MWNumericArray}

import scala.concurrent.duration._
import scala.swing.event.Event
import scala.swing.{Point, Publisher}

case object DataFinishedEvent extends Event

case object BeginCalculatingEvent extends Event

case object NeedRefreshEvent extends Event

case object NotNeedRefreshEvent extends Event

/**
 * Created by chiewen on 8/8/14.
 */
object DemoData extends Publisher {
  var points: List[NeighboredSiteMemory] = null
  var voronoi: List[Array[(Double, Double)]] = null
  var tree: VTree = null
  var k: Int = 0
  var rho: Double = 1
  var rnn: List[NeighboredSiteMemory] = null
  var ins: List[NeighboredSiteMemory] = null
  var clip: List[Array[(Double, Double)]] = null

  def knn = if (rnn == null) null else rnn.take(k)

  private var _query: Point = null

  def query = _query

  def query_=(p: Point) {
    _query = p

    if (rnn != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (_query.x, _query.y)) < pointsDistanceNS(b, (_query.x, _query.y))
      implicit val ordering = Ordering.fromLessThan(isNearer)
      // knn remain the same
      val far = max(knn)
      if (ins.exists(p => isNearer(p, far))) publish(NeedRefreshEvent)
      else publish(NotNeedRefreshEvent)
    }
    else refresh
  }

  def refresh {
    if (rnn != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (_query.x, _query.y)) < pointsDistanceNS(b, (_query.x, _query.y))
      implicit val ordering = Ordering.fromLessThan(isNearer)
      // knn remain the same
      val far = max(knn)
      if (ins.exists(p => isNearer(p, far))) recalculateKnn
    }
    else recalculateKnn

    def recalculateKnn {
      publish(BeginCalculatingEvent)

      rnn = tree.knn((k * rho).toInt, (_query.x, _query.y))._1
      // TODO confirm the function of the "distinct" operation
      ins = (rnn.flatMap(f => f.getNeighbors) ::: rnn).distinct.filterNot(f => knn.exists(e => e.id == f.id))
      val pointsExcluded = points.filterNot(p => knn.exists(e => e.id == p.id))
      clip = Nil

      implicit val ec = global
      val system = ActorSystem("default")
      val actor = system.actorOf(Props(new OrderKVoronoiActor))
      implicit val timeout = Timeout(25 seconds)
      val future = actor ? StartCalcOrderK(knn, pointsExcluded)
      future.map { result =>
        clip = result.asInstanceOf[List[Array[(Double, Double)]]]
        system.shutdown()
        publish(DataFinishedEvent)
      }
    }
  }

  def max(list: List[NeighboredSiteMemory]) = {
    if (_query != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (_query.x, _query.y)) < pointsDistanceNS(b, (_query.x, _query.y))
      implicit val ordering = Ordering.fromLessThan(isNearer)
      list.max
    }
    else null
  }

  def min(list: List[NeighboredSiteMemory]) = {
    if (_query != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (_query.x, _query.y)) < pointsDistanceNS(b, (_query.x, _query.y))
      implicit val ordering = Ordering.fromLessThan(isNearer)
      list.min
    }
    else null
  }

  def reset(num: Int, k: Int, rho: Double, width: Int, height: Int) {
    points = SiteGenerator.getSites(num, width, height - 25)
    voronoi = calcVoronoi(points, width, height)
    tree = VTree(points)
    this.k = k
    this.rho = rho
    _query = null
    rnn = null
    ins = null
    clip = null
  }

  def voronoi(ps: List[NeighboredSiteMemory], width: Int = 300, height: Int = 300): Array[Object] = {
    var points = ps.toList

    //add far points
    val i = points.size
    val FAR = 10000
    points ::= new NeighboredSiteMemory(i + 1, (-1 * FAR, -1 * FAR))
    points ::= new NeighboredSiteMemory(i + 2, (width / 2, -1 * FAR))
    points ::= new NeighboredSiteMemory(i + 3, (FAR, -1 * FAR))
    points ::= new NeighboredSiteMemory(i + 4, (-1 * FAR, height / 2))
    points ::= new NeighboredSiteMemory(i + 5, (FAR, height / 2))
    points ::= new NeighboredSiteMemory(i + 6, (-1 * FAR, FAR))
    points ::= new NeighboredSiteMemory(i + 7, (width / 2, FAR))
    points ::= new NeighboredSiteMemory(i + 8, (FAR, FAR))

    val array = Array(points.length, 2)
    val x = MWNumericArray.newInstance(array,
      MWClassID.DOUBLE, MWComplexity.REAL)

    points.zipWithIndex.foreach { p =>
      x.set(Array(p._2 + 1, 1), p._1.coordinates(0)._1)
      x.set(Array(p._2 + 1, 2), p._1.coordinates(1)._1)
    }

    var result: Array[Object] = null

    try {
      //the type of r(0) is [MWNumericArray]
      //the type of r(1) is [MWArray] that contains [Array[Array[Double]]]
      //r(1) can be used as: r(1).asInstanceOf[MWArray].get(Array(p.id, 1)).asInstanceOf[Array[Array[Double]]](0)
      result = new MatlabPlotter().calcVoronoi(2, x)
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
    }
    result
  }

  def singleVoronoi(ps: List[NeighboredSiteMemory]) = {
    val result = voronoi(ps)
    result(1).asInstanceOf[MWArray].get(Array(9, 1)).asInstanceOf[Array[Array[Double]]](0)
      .map(d => (result(0).asInstanceOf[MWNumericArray].getDouble(Array(d.toInt, 1)),
      result(0).asInstanceOf[MWNumericArray].getDouble(Array(d.toInt, 2))
      ))
  }

  def calcVoronoi(ps: List[NeighboredSiteMemory], width: Int = 300, height: Int = 300) = {
    val result = voronoi(ps, width, height)

    (1 to ps.size + 8).map(p =>
      result(1).asInstanceOf[MWArray].get(Array(p, 1)).asInstanceOf[Array[Array[Double]]](0)
        .map(d => (result(0).asInstanceOf[MWNumericArray].getDouble(Array(d.toInt, 1)),
        result(0).asInstanceOf[MWNumericArray].getDouble(Array(d.toInt, 2))
        ))
    ).toList
  }
}
