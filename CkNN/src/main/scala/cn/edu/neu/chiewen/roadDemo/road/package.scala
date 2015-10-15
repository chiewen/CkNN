package cn.edu.neu.chiewen.roadDemo

/**
 * Created by chiewen on 2015/9/19 9:28.
 */
package object road {
  private var id = 0L

  def setId(i: Long) {
    id = i
  }

  def getID: Long = {
    id += 1
    id
  }


}
