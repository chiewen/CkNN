package cn.edu.neu.chiewen.cknn.algorithms.vsdiagram

import cn.edu.neu.chiewen.cknn.algorithms.Util._
import cn.edu.neu.chiewen.cknn.client.Client
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.site.Site

import scala.beans.BeanProperty
import scala.collection.mutable.ListBuffer

class VsClient extends Client {
  @BeanProperty var k: Int = 0
  @BeanProperty var x: Int = 0

  var qb: (Double, Double) = null
  var L: List[Site] = null
  var disToZ: Double = 0
  var pk: Site = null
  var m = 0
  //mbr retrieval count
  var c = 0
  //communication count
  var t = 0
  //traffic burden
  var f = 0
  //knn call count
  var km = 0
  //knn miss
  var out = 0 //out of current cell time

  def reset {
    c = 0; m = 0; t = 0; st = 0L; ct = 0L; disToZ = 0; f = 0; L = null; qb = null; pk = null; km = 0; out = 0
  }

  def retrievalCount: Int = m

  def communicationCount: Int = c

  def trafficCount: Int = t

  override def knnMiss: Int = km

  def knn: List[Site] = L.take(k)

  //k,x,f
  def describe: String = "Vs\t" + "%2d".format(k) + "\t" + "%5d".format(x) + "\t" + "%5d".format(out) + "\t" + "%3d".format(f)

  def check(position: (Double, Double))(implicit server: Server) {
    |<<
    if (L == null) {
      qb = position
      computeVS
    }

    val rankResult = isRankUpdated(position)
    val reliabilityResult = isReliabilityUpdated(position)
    if (reliabilityResult) {
      qb = position
      out += 1
      computeVS
    } else if (rankResult._1) {
      val lb = ListBuffer.empty[Site]
      lb.append(L.take(rankResult._2): _*)
      lb.append(L(rankResult._2 + 1))
      lb.append(L(rankResult._2))
      lb.append(L.takeRight(L.length - rankResult._2 - 2): _*)
      L = lb.toList

      if (rankResult._2 == k - 2 || rankResult._2 == k - 1) pk = L(k - 1)
    }
    >>|
  }

  private def computeVS()(implicit server: Server) {
    >>|<
    km += 1
    val result = server.getKNN(k + x, qb)
    >|<<
    L = result._1
    m += result._2
    c += 1
    t += L.size
    f += 1
    disToZ = pointsDistance(L(k + x - 1), qb)
    pk = L(k - 1)
  }

  def insert(position: (Double, Double), inserted: Site)(implicit server: Server) {
    def isNearer(a: Site, b: Site) = pointsDistanceNS(a, position) < pointsDistanceNS(b, position)
    implicit val ordering = Ordering.fromLessThan(isNearer)

    if (disToZ < pointsDistance(inserted, qb)) return

    L ::= inserted
    L = L.sorted.take(k + x)
    pk = L(k - 1)

    if (isNearer(inserted, L(k)) && isReliabilityUpdated(position)) computeVS
  }

  def delete(position: (Double, Double), deleted: Site)(implicit server: Server) {
    def isNearer(a: Site, b: Site) = pointsDistanceNS(a, position) < pointsDistanceNS(b, position)
    implicit val ordering = Ordering.fromLessThan(isNearer)

    if (disToZ < pointsDistance(deleted, qb)) return

    L = L.filterNot(p => p.id == deleted.id)
    L = L.sorted
    if (L.length < k) {
      qb = position
      computeVS
    }
    pk = L(k - 1)
    if (isNearer(deleted, L(k)))
    //&& isReliabilityUpdated(position))
      computeVS
  }

  def get(i: Int) = L(i)

  private def isReliabilityUpdated(position: (Double, Double)) =
    pointsDistance(position, qb) + pointsDistance(position, pk) >= disToZ

  private def isRankUpdated(position: (Double, Double)) = {
    var isUpdated = false
    var updatedIndex = -1

    for (i <- (0 to L.length - 2))
      if (pointsDistance(L(i), position) > pointsDistance(L(i + 1), position)) {
        isUpdated = true
        updatedIndex = i
      }
    (isUpdated, updatedIndex)
  }
}