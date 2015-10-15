package cn.edu.neu.chiewen.roadDemo.ui

/**
 * Created by chiewen on 2015/9/22 21:10.
 */
object PanelDrawingState {
  var showRoad = false
  var showNode = false
  var showKnn = false
  var showNeighbors = false

  def setShow(road: Boolean, node: Boolean, knn: Boolean, neighbors: Boolean): Unit = {
    showRoad = road
    showNeighbors = neighbors
    showNode = node
    showKnn = knn
  }
}
