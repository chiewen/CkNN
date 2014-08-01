package cn.edu.neu.chiewen.cknn.algorithms.cknn

import cn.edu.neu.chiewen.cknn.algorithms.Util.pointsDistanceNS
import cn.edu.neu.chiewen.cknn.client.Client
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.site.NeighboredSite
import cn.edu.neu.chiewen.cknn.site.Site.Site2DoubleTuple

import scala.beans.BeanProperty

/**
 * the algorithm corresponding to the paper.
 */
class CkNNClient extends Client {
  @BeanProperty var k = 0
  var c = 0
  //communication count
  var t = 0
  //traffic burden
  var sr = 0
  //mbr retrieval count
  var nr = 0
  //neighbor retrieval count
  var f = 0
  //knn call count
  var km = 0
  //knn miss
  var out = 0
  //out of current cell time
  var _knn: List[NeighboredSite] = Nil
  var _neighbors: List[NeighboredSite] = Nil

  def reset {
    c = 0
    t = 0
    sr = 0
    nr = 0
    f = 0
    st = 0L
    ct = 0L
    _knn = Nil
    km = 0
    out = 0
  }

  def knn: List[NeighboredSite] = _knn

  def retrievalCount: Int = sr + nr

  def communicationCount: Int = c

  def trafficCount: Int = t

  override def knnMiss: Int = km

  //k,n,f
  def describe: String = "C6\t" + "%2d".format(k) + "\t" + "%5d".format(nr) + "\t" + "%5d".format(out) + "\t" + "%3d".format(f)

  def check(position: (Double, Double))(implicit server: Server) {
    def isNearer(a: NeighboredSite, b: NeighboredSite): Boolean =
      pointsDistanceNS(a, position) < pointsDistanceNS(b, position)

    implicit val ordering = Ordering.fromLessThan(isNearer)

    |<<
    if (_knn.isEmpty) {
      >>|<
      refreshKnn(position, server)
      >|<<
    }

    val nn = _neighbors.min
    val kf = _knn.max

    if (isNearer(nn, kf)) {
      >>|<
      //out += 1

      _knn = _knn.filterNot(p => p.id == kf.id)
      _knn ::= nn

      _neighbors = Nil
      for (i <- _knn; n <- i.getNeighbors if !_knn.exists(n.id == _.id) && !_neighbors.exists(n.id == _.id)) {
        _neighbors ::= n
      }

      if (!isNearer(_neighbors.min, _knn.max)) {
        t += 6 //the server maintain the neighbors set of the client, so it know which procedure should be executed
        c += 1
        >|<<
      } else {
        refreshKnn(position, server)
        >|<<
      }
    }
    >>|
  }

  def insert(inserted: NeighboredSite, position: (Double, Double)) {
    def isNearer(a: NeighboredSite, b: NeighboredSite): Boolean = pointsDistanceNS(a, position) < pointsDistanceNS(b, position)
    implicit val ordering = Ordering.fromLessThan(isNearer)
    //if (_knn.contains(inserted)) return

    val kf = _knn.max
    if (isNearer(inserted, kf)) {
      _knn = _knn.filterNot(p => p.id == kf.id)
      _knn ::= inserted
    } else _neighbors ::= inserted
  }

  def delete(deleted: NeighboredSite, position: (Double, Double))(implicit server: Server) {
    def isNearer(a: NeighboredSite, b: NeighboredSite): Boolean = pointsDistanceNS(a, position) < pointsDistanceNS(b, position)
    implicit val ordering = Ordering.fromLessThan(isNearer)

    if (_knn.exists(p => p.id == deleted.id)) {
      val n = _neighbors.min

      _knn = _knn.filterNot(p => p.id == deleted.id)
      _knn ::= n

      _neighbors :::= n.getNeighbors
      _neighbors = _neighbors.filterNot(p => _knn.contains(p))
    } else {
      _neighbors :::= deleted.getNeighbors
      _neighbors = _neighbors.filterNot(p => _knn.contains(p))
    }
  }

  private def refreshKnn(position: (Double, Double), server: Server): Unit = {
    implicit val s = server
    val result = server.getKNN(k, position)
    km += 1

    _knn = result._1.asInstanceOf[List[NeighboredSite]]
    _neighbors = Nil
    for (n <- _knn; s <- n.getNeighbors if !_knn.exists(s.id == _.id) && !_neighbors.exists(s.id == _.id)) {
      _neighbors ::= s
    }
    //_neighbors = _neighbors.distinct.diff(_knn)

    val nc = _neighbors.length
    sr += result._2
    nr += 0
    t += nc + k
    c += 1
    f += 1
  }

  def deleteNeighbor(id: Int) {
    _neighbors = _neighbors.filterNot(p => p.id == id)
  }

  def get(i: Int) = {
    //    print("knn:"+ _knn.length + "   i:" + i + " id:" + _knn(i).id + "   :")
    //    for (p <- _knn) print(p.id + ";  ")
    //    println()
    _knn(i)
  }
}