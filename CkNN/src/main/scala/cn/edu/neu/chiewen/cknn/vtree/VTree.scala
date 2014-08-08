package cn.edu.neu.chiewen.cknn.vtree

import Plotter.MatlabPlotter
import cn.edu.neu.chiewen.cknn.algorithms.Util.DoubleTuple2Mbr
import cn.edu.neu.chiewen.cknn.site.NeighboredSiteMemory
import cn.edu.neu.chiewen.rtree.core.{EntryNode, Node, RsTree, TreeNode}
import com.mathworks.toolbox.javabuilder.{MWArray, MWClassID, MWComplexity, MWNumericArray}

import scala.collection.mutable

/**
 * The VTree adds Voronoi neighbors to each entry node of the r*-tree.
 * Only support 2D distributions, because of the limitation of the matlab <code>voronoin</code> function.
 *
 * @author LI Chuanwen
 *
 */
class VTree(points: List[NeighboredSiteMemory]) extends RsTree with Serializable {
  points foreach insert

  /**
   * Implementing the BF-kNN algorithm introduced in the V*-Diagram paper
   */
  def knn(k: Int, p: (Double, Double)) = {
    var c = 1
    val mbrs = mutable.PriorityQueue.empty(Ordering.fromLessThan((a: Node, b: Node) =>
      p.distanceToNS(a.mbr) > p.distanceToNS(b.mbr)))

    mbrs.enqueue(getRoot)

    var result: List[NeighboredSiteMemory] = Nil
    while (result.length < k && mbrs.nonEmpty)
      mbrs.dequeue() match {
        case t: EntryNode =>
          result ::= t.mbr.asInstanceOf[NeighboredSiteMemory]; c += 1
        case t: TreeNode => mbrs.enqueue(t.children: _*); c += t.children.length
      }
    (result.reverse, c)
  }

  /**
   * Implementing the BF-kNN algorithm introduced in the V*-Diagram paper
   */
  def nnNode(p: (Double, Double)) = {
    val mbrs = mutable.PriorityQueue.empty(Ordering.fromLessThan((a: Node, b: Node) =>
      p.distanceToNS(a.mbr) > p.distanceToNS(b.mbr)))

    mbrs.enqueue(getRoot)

    var r: Node = null
    while (r == null)
      mbrs.dequeue() match {
        case t: EntryNode =>
          r = t
        case t: TreeNode => mbrs.enqueue(t.children: _*)
      }
    r
  }

  /**
   * Implementing the BF-kNN algorithm introduced in the V*-Diagram paper
   */
  def fnNode(p: (Double, Double)) = {
    val mbrs = mutable.PriorityQueue.empty(Ordering.fromLessThan((a: Node, b: Node) =>
      p.distanceFarToNS(a.mbr) < p.distanceFarToNS(b.mbr)))

    mbrs.enqueue(getRoot)

    var r: Node = null
    while (r == null)
      mbrs.dequeue() match {
        case t: EntryNode =>
          r = t
        case t: TreeNode => mbrs.enqueue(t.children: _*)
      }
    r
  }
}

object VTree {
  /**
   * The VorTree implementation requires all the Mbr's in the source RsTree to be Point style.
   * The main purpose of the apply function is to set proper Voronoi neighbors for each point.
   */
  def apply(points: List[NeighboredSiteMemory]) = {
    val array = Array(points.length, 2)
    val x = MWNumericArray.newInstance(array,
      MWClassID.DOUBLE, MWComplexity.REAL)

    points.zipWithIndex.foreach { p =>
      x.set(Array(p._2 + 1, 1), p._1.coordinates(0)._1)
      x.set(Array(p._2 + 1, 2), p._1.coordinates(1)._1)
    }

    try {
      //the type of r(0) is [MWNumericArray]
      //the type of r(1) is [MWArray] that contains [Array[Array[Double]]]
      //r(1) can be used as: r(1).asInstanceOf[MWArray].get(Array(2, 1)).asInstanceOf[Array[Array[Double]]]
      val r = new MatlabPlotter().calcVoronoi(2, x)

      val map = new mutable.HashMap[Int, Set[Int]]
      var i1 = 0
      var n1 = 0
      for ( //c.toInt == 1 means the point expands to infinite, indicating the point is at the border of the graph.
        p <- points;
        c <- r(1).asInstanceOf[MWArray].get(Array(p.id, 1)).asInstanceOf[Array[Array[Double]]](0) if c.toInt != 1
      ) {
        if (!map.contains(c.toInt)) map(c.toInt) = Set(p.id)
        else map(c.toInt) += p.id
        i1 += 1
        if (i1 % 500 == 0) println("i1 " + i1)
      }

      val kn = map.keys.size
      for (k <- map.keys) {
        for (
          s <- map(k);
          n <- map(k) if n != s
        ) points(s - 1).neighbors += points(n - 1)
        n1 += 1
        if (n1 % 500 == 0) println("n1 " + kn + ": " + n1)
      }
    } catch {
      case e: Exception => System.out.println("Exception: " + e.toString);
    } finally {
      MWArray.disposeArray(x)
    }
    new VTree(points)
  }
}