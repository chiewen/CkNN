package cn.edu.neu.chiewen.rtree.core

import scala.collection.mutable.ListBuffer
import Math._

/**
 * For using in a high dimensional circumstance, the bounds of MBR use Array instead of just x and y.
 */
class Mbr(val coordinates: List[(Double, Double)], val mid: Long = Mbr.nextId) extends Serializable {
  override def toString = {
    val sb = new StringBuilder("<")
    (sb /: coordinates) { (sb, n) => sb.append(n.toString) }
    sb.append(">").toString
  }

  /**
   * The concept of area is extended when the dimension exceeds 2.
   */
  def area = (1.0 /: coordinates) { (product, range) => product * (range._2 - range._1) }

  /**
   * Since the margin concept is only for the comparing purpose, we omit the factor of 2^(dimension - 1)
   */
  def magin = (0.0 /: coordinates) { (sum, range) => sum + (range._2 - range._1) }

  /**
   * Get the tightest MBR that contains the original MBR and the newly inserted one.
   */
  def unionBy(inserted: Mbr) = Mbr.union(ListBuffer(this, inserted))

  /**
   * The MBR area enlarged by the other one
   */
  def enlargedBy(inserted: Mbr) = unionBy(inserted).area - this.area

  /**
   * Whether this MBR overlaps with the other
   */
  def isOverlap(m: Mbr) = !(coordinates zip m.coordinates).exists(n => (n._2._1 >= n._1._2 || n._1._1 >= n._2._2))

  /**
   * The are overlapped by the other
   */
  def overlappedArea(m: Mbr) = (1.0 /: (coordinates zip m.coordinates)) { (r, c) =>
    r * (
      Math.min(c._1._2, c._2._2) - Math.max(c._1._1, c._2._1) match {
        case i if i <= 0 => 0
        case i => i
      })
  }

  def center = coordinates map { c => (c._2 + c._1) / 2 }

  /**
   * The minimal distance to another Mbr. The result would be 0 if the two Mbr's are overlapped
   */
  def distanceTo(m: Mbr) =
    sqrt((0.0 /: (coordinates zip m.coordinates)) { (sum, n) =>
      pow(max(max(n._2._1 - n._1._2, n._1._1 - n._2._2), 0), 2) + sum
    })
    
  /**
   * The non-Sqrt minimal distance to another Mbr. For the purpose of compare.
   */
  def distanceToNS(m: Mbr) =
    (0.0 /: (coordinates zip m.coordinates)) { (sum, n) =>
      pow(max(max(n._2._1 - n._1._2, n._1._1 - n._2._2), 0), 2) + sum
    }
    
  /**
   * The non-Sqrt minimal distance to another Mbr. For the purpose of compare.
   */
  def distanceFarToNS(m: Mbr) =
    (0.0 /: (coordinates zip m.coordinates)) { (sum, n) =>
      pow(max(n._2._2 - n._1._1, n._1._2 - n._2._1), 2) + sum
    }

}

object Mbr {
  /**
   * The tightest MBR that contains the input series of MBRs
   */
  def union(mbrs: ListBuffer[Mbr]) =
    new Mbr((mbrs transpose (_.coordinates) map
      (d => ((d minBy ((_: (Double, Double))._1))._1, (d maxBy ((_: (Double, Double))._2))._2)))
      .toList)

  def apply(c: (Double, Double)*) = new Mbr(c.toList)

  var id = 0L
  def nextId = { id += 1; id }
}