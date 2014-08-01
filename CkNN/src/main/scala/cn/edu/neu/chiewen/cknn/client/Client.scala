package cn.edu.neu.chiewen.cknn.client

import cn.edu.neu.chiewen.cknn.Result
import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.cknn.site.Site

trait Client extends Result {
  var k: Int

  var st: Long = 0
  //server time
  var ct: Long = 0 //client time

  //server time start
  def |< {
    st -= System.nanoTime
  }

  //server time end
  def >| {
    st += System.nanoTime
  }

  //client time start
  def |<< {
    ct -= System.nanoTime
  }

  //client time end
  def >>| {
    ct += System.nanoTime
  }

  //switch counting server time to client time
  def >|<< {
    st += System.nanoTime; ct -= System.nanoTime
  }

  //switch counting client time to server time 
  def >>|< {
    ct += System.nanoTime; st -= System.nanoTime
  }

  def retrievalCount: Int

  def communicationCount: Int

  def trafficCount: Int

  def knn: List[Site]

  def check(position: (Double, Double))(implicit server: Server)

  def describe: String

  def reset: Unit

  def knnMiss: Int = 0

  def correctness: Double = 0

  def showResult(s: String)

}