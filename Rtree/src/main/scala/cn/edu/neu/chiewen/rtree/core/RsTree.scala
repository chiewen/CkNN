package cn.edu.neu.chiewen.rtree.core

import scala.collection.mutable.ListBuffer

/**
 * According to Beckmann's paper: "The R*-tree: an efficient and robust access method for points and rectangles"
 * <p>
 * @author <a href="mailto:lichuanwen@ise.neu.edu.cn">LI Chuanwen</a>
 * During the winter vacation of 2012 in Yantai
 */
class RsTree extends Serializable {
  private var root: TreeNode = TreeNode(null, ListBuffer[Node](), true)
  def getRoot = root

  var M = 8
  val m = (M * .4).ceil.toInt
  val pr = (M * .3).ceil.toInt

  /**
   * Insert new entry into the tree
   * @param entry the entry to be inserted
   */
  def insert(entry: Mbr) {
    val accommodation = chooseLeafToInsert(root, entry)
    accommodation.children += new EntryNode(accommodation, entry)
    adjustTree(accommodation, List[Int]())
  }

  /**
   * After an insertion, the parent node must be checked to see whether its children amount exceeds M.
   * If so, the node should be split into two. A recursive call on the parent would be necessary
   * if a splitting operation is performed on a node.
   */
  def adjustTree(n: TreeNode, r: List[Int]) {
    if (n.children.length <= M) { n.refreshMbr; return }
    if (n eq root) {
      n.parent = TreeNode(null, ListBuffer[Node](), false)
      root = n.parent
      root.children += n //ensuring that the root always has children, which is required by the <code>dimension</code> method
    }

    if (!r.contains(currentLayer(n)) && currentLayer(n) > 1) { //force reinsert
      def dist(s: Node) = (1.0 /: (n.mbr.center zip s.mbr.center)) { (p, c) => p + (c._1 - c._2) * (c._1 - c._2) }
      val sorted = n.children sortBy dist

      n.children = sorted.take(M + 1 - pr)
      n.refreshMbr

      sorted takeRight pr foreach { f =>
        (f.parent.parent.children minBy { (_).mbr.enlargedBy(f.mbr) }).asInstanceOf[TreeNode].addChild(f)
        adjustTree(f.parent, currentLayer(n) :: r)
      }
    } else {
      n.parent.children ++= splitNode(n) -= n
      adjustTree(n.parent, r)
    }
  }

  def currentLayer(n: Node): Int = n match {
    case _ if n eq root => 0
    case _ => 1 + currentLayer(n.parent)
  }

  /**
   * Choose which node should be inserted
   * @param entry the entry to be inserted
   */
  def chooseLeafToInsert(n: TreeNode, entry: Mbr): TreeNode =
    n match {
      case t: TreeNode if (t.isLeaf) => n //if the Node is leaf 
      case p: TreeNode if (p.children(0).asInstanceOf[TreeNode].isLeaf) =>
        p.children.minBy((_: Node).asInstanceOf[TreeNode].overlappedArea(entry)).asInstanceOf[TreeNode]
      case _ =>
        chooseLeafToInsert(n.children.minBy((_: Node).mbr.enlargedBy(entry)).asInstanceOf[TreeNode], entry)
    }

  /**
   * Split the node when its children amount exceeds M
   */
  def splitNode(n: TreeNode) = {
    assert(n.children.length == M + 1)

    val axis = (((ListBuffer[Node](), ListBuffer[Node]()), Double.MaxValue) /: (0 until dimension)) { (md, i) =>
      {
        val l = n.children sortBy ((_: Node).mbr.coordinates(i)._1)
        val r = n.children sortBy ((_: Node).mbr.coordinates(i)._2)

        val s = (0.0 /: (m to M + 1 - m)) { (sum, j) =>
          (sum /: List(l.take(j), l.takeRight(M + 1 - j), r.take(j), r.takeRight(M + 1 - j))) {
            (s, b) => s + Mbr.union(b map (_.mbr)).magin
          }
        }

        if (s < md._2) ((l, r), s) else md
      }
    }

    def olp(b: ListBuffer[Node]) =
      (0.0 /: b.map(_.mbr)) { (sum, m) => sum + Mbr.union(b.map(_.mbr)).overlappedArea(m) }
    def colp(n: (ListBuffer[Node], ListBuffer[Node])) = olp(n._1) + olp(n._2)

    val a = (m to M + 1 - m).flatMap { i =>
      List((axis._1._1.take(i), axis._1._1.takeRight(M + 1 - i)), (axis._1._2.take(i), axis._1._2.takeRight(M + 1 - i)))
    }.minBy(colp(_))

    List(TreeNode(n.parent, a._1, n.isLeaf), TreeNode(n.parent, a._2, n.isLeaf))
  }

  /**
   * Find the entries which overlap with the particular MBR
   */
  def findLeaf(mbr: Mbr): List[EntryNode] = {
    def _findLeaf(node: Node, mbr: Mbr): List[EntryNode] =
      node match {
        case e: EntryNode => if (mbr isOverlap e.mbr) List(e) else List[EntryNode]()
        case t: TreeNode => t.children.filter { n => n.mbr isOverlap mbr }
          .foldLeft(List[EntryNode]()) { (l, n) => l union _findLeaf(n, mbr) }
      }
    _findLeaf(root, mbr)
  }

  /**
   * Delete the particular node
   */
  def delete(n: Node) {
    n.parent.children -= n
    condenseTree(n.parent, List[Int]())
  }

  /**
   * After a node is deleted, we should check if its parent node holds less than m children.
   * If so, the tree should be condensed. We should propagate the condensing operation upward
   * till the capacity criteria are satisfied at every layer.
   */
  def condenseTree(n: TreeNode, r: List[Int]) {
    n.refreshMbr
    lowerTreeWhenRootHasOnlyOneChild(n)
    /*
     * Here we check whether <code>n.parent != null</code> instead of <code>n != root</code>, 
     * since the <code>lowerTreeWhenRootHasOnlyOneChild</code> method may make a root be a non-root. 
     * However, if <code>n</code> is the root previously, its parent remains null.
     */
    if (n.children.length < m && n.parent != null) {
      n.parent.children -= n
      n.children foreach { o =>
        (n.parent.children minBy (_.mbr.enlargedBy(o.mbr))).asInstanceOf[TreeNode] addChild o
        adjustTree(o.parent, r)
      }
      condenseTree(n.parent, r)
    }
  }

  private def lowerTreeWhenRootHasOnlyOneChild(n: TreeNode) {
    if (n.eq(root) && n.children.length == 1 && n.children(0).isInstanceOf[TreeNode]) {
      root = n.children(0).asInstanceOf[TreeNode]
      n.children(0).parent = null
    }
  }

  private def dimension = {
    def dive(n: Node): Int = n match {
      case e: EntryNode => e.mbr.coordinates.length
      case _ => dive(n.asInstanceOf[TreeNode].children(0))
    }
    dive(root)
  }

  def entries = {
    var m = List[Mbr]()
    def traverseTree(e: Node) {
      e match {
        case t: TreeNode => for (i <- t.children) traverseTree(i)
        case e: EntryNode => m ::= e.mbr
      }
    }
    traverseTree(root)
    m
  }

}