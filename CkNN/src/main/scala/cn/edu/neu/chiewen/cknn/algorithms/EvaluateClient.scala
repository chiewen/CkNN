package cn.edu.neu.chiewen.cknn.algorithms

import cn.edu.neu.chiewen.cknn.algorithms.Util.pointsDistanceNS
import cn.edu.neu.chiewen.cknn.client.Client
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.site.Site

import scala.collection.immutable.List

class EvaluateClient(val client: Client) extends Client {
  client.reset

  var k: Int = client.k

  var all = 0.0
  var correct = 0

  def reset {
    all = 0
    correct = 0
    st = 0L
    ct = 0L
  }

  def retrievalCount: Int = client.retrievalCount

  def communicationCount: Int = client.communicationCount

  def trafficCount: Int = client.trafficCount

  override def knnMiss: Int = client.knnMiss

  override def correctness: Double = correct / all

  def check(position: (Double, Double))(implicit server: Server) {

    def isNearer(a: Site, b: Site): Boolean = pointsDistanceNS(a, position) < pointsDistanceNS(b, position)
    implicit val ordering = Ordering.fromLessThan(isNearer)

    client.check(position)
    ct = client.ct
    st = client.st

    //all += 1
    //val sknn = server.getKNN(k, position)._1
    //val kn = knn
    //val s1 = sknn.length
    //val k1 = kn.length
    //val n = correct
    //if (knn.sorted == sknn) correct += 1
    //else {print("!!!");print(knn);print("->");print(sknn);}
  }

  def knn: List[Site] = client.knn

  //st,ct,r,c,t,accuracy
  def describe: String = client.describe + "\t" + "%10.3f".format(st / 1000000.0) + "\t" + "%10.3f".format(ct / 1000000.0) + "\t" + "%6d".format(retrievalCount) + "\t" + "%6d".format(knnMiss) + "\t" + "%5d".format(communicationCount) + "\t" + "%5d".format(trafficCount) + "\t" + "%7.2f".format(correct / all * 100)
}