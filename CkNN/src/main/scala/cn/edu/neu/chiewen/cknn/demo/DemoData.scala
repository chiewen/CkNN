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

//case object DataFinishedEvent extends Event

//case object BeginCalculatingEvent extends Event

//case object NeedRefreshEvent extends Event

//case object NotNeedRefreshEvent extends Event

case object RepaintEvent extends Event


/**
 * Created by chiewen on 8/8/14.
 */
object DemoData extends Publisher {
  val system = ActorSystem("default")

  var isCalculating = false
  var needRefresh = false

  var points: List[NeighboredSiteMemory] = Nil
  var voronoi: List[Array[(Double, Double)]] = Nil
  var tree: VTree = null
  var k: Int = 0
  var rho: Double = 1
  var rnn: List[NeighboredSiteMemory] = null
  var ins: List[NeighboredSiteMemory] = null
  var all: List[NeighboredSiteMemory] = null
  var clip: List[Array[(Double, Double)]] = Nil
  var query: Point = null
  var _auto = true

  def auto = _auto

  def auto_=(a: Boolean) {
    _auto = !a
    if (_auto) clip = Nil
    validate
    publish(RepaintEvent)
  }

  def knn = if (rnn == null) null else rnn.take(k)

  def validate {
    if (auto) {
      if (!isValid) refresh()
    }
    else {
      if (isValid) {
        needRefresh = false
        if (clip.isEmpty) calcClip
      }
      else needRefresh = true
      publish(RepaintEvent)
    }
  }

  def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
    pointsDistanceNS(a, (query.x, query.y)) < pointsDistanceNS(b, (query.x, query.y))

  implicit val ordering = Ordering.fromLessThan(isNearer)

  def isValid: Boolean = {
    if (rnn == null) recalculateKnn()
    if (ins.exists(p => isNearer(p, max(knn)))) false else true
  }

  def refresh() {
    val firstK = all.sorted.take(k)
    if (firstK.forall(f => rnn.exists(r => r.id == f.id))) {
      needRefresh = false
      publish(RepaintEvent)
      rnn = rnn.sorted
      ins = all.filterNot(f => knn.exists(r => f.id == r.id))
    }
    else recalculateKnn()
    calcClip
  }

  def calcClip {
    if (_auto) return

    val pointsExcluded = points.filterNot(p => knn.exists(e => e.id == p.id))
    clip = Nil

    implicit val ec = global()
    val actor = system.actorOf(Props(new OrderKVoronoiActor))
    implicit val timeout = Timeout(10 seconds)
    val future = actor ? StartCalcOrderK(knn, pointsExcluded)
    future.map { result =>
      clip = result.asInstanceOf[List[Array[(Double, Double)]]]
      isCalculating = false
      publish(RepaintEvent)
    }
  }

  def recalculateKnn() {
    needRefresh = false
    isCalculating = true
    publish(RepaintEvent)

    rnn = tree.knn((k * rho).toInt, (query.x, query.y))._1
    all = (rnn.flatMap(f => f.getNeighbors) ::: rnn).distinct

    // TODO confirm the function of the "distinct" operation
    ins = all.filterNot(f => knn.exists(e => e.id == f.id))
  }

  def max(list: List[NeighboredSiteMemory]) = {
    if (query != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (query.x, query.y)) < pointsDistanceNS(b, (query.x, query.y))
      implicit val ordering = Ordering.fromLessThan(isNearer)
      list.max
    }
    else null
  }

  def min(list: List[NeighboredSiteMemory]) = {
    if (query != null) {
      def isNearer(a: NeighboredSiteMemory, b: NeighboredSiteMemory): Boolean =
        pointsDistanceNS(a, (query.x, query.y)) < pointsDistanceNS(b, (query.x, query.y))
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
    query = null
    rnn = null
    ins = null
    all = null
    clip = Nil
  }

  def voronoi(ps: List[NeighboredSiteMemory], width: Int = 300, height: Int = 300): Array[Object] = {
    var points = ps.toList

    //add far points
    val i = points.size

    //the Voronoi diagram will include wired additional lines. The smaller the 'FAR' is, the more wired lines occur.
    //Looks like some bug in Matlab.
    val FAR = 100000
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
