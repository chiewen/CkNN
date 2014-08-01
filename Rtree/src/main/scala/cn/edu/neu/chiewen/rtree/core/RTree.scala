package cn.edu.neu.chiewen.rtree.core

import scala.collection.mutable.ListBuffer

/**
 * According to Guttman's paper: "R-Trees: A dynamic index structure for spatial searching"
 * <p>
 * @author <a href="mailto:lichuanwen@ise.neu.edu.cn">Chuanwen LI</a>
 * During the winter vacation of 2012 in Yantai
 */
class RTree {
  private var root: TreeNode = TreeNode(null, new ListBuffer[Node], true)
  def getRoot = root

  val M = 8
  val m = M / 2

  /**
   * Insert new entry into the tree
   * @param entry the entry to be inserted
   */
  def insert(entry: Mbr) {
    val accommodation = chooseLeafToInsert(root, entry)
    accommodation.children += new EntryNode(accommodation, entry)
    adjustTree(accommodation)
  }

  /**
   * After an insertion, the parent node must be checked to see whether its children amount exceeds M.
   * If so, the node should be split into two. A recursive call on the parent would be necessary
   * if a splitting operation is performed on a node.
   */
  def adjustTree(n: TreeNode) {
    if (n.children.length <= M) { n.refreshMbr; return }
    if (n eq root) {
      n.parent = TreeNode(null, new ListBuffer[Node], false)
      root = n.parent
    }
    n.parent.children -= n ++= splitNode(n)
    adjustTree(n.parent)
  }
  /**
   * Choose which node should be inserted
   * @param entry the entry to be inserted
   */
  def chooseLeafToInsert(n: TreeNode, entry: Mbr): TreeNode =
    n match {
      case t: TreeNode if (t.isLeaf == true) => n //if the Node is leaf 
      case _ =>
        chooseLeafToInsert((n.children minBy ((_: Node).mbr.enlargedBy(entry))).asInstanceOf[TreeNode], entry)
    }

  /**
   * Split the node when its children amount exceeds M
   */
  def splitNode(n: TreeNode) = {
    val seeds = PickSeeds(n.children)
    val result = List(TreeNode(n.parent, ListBuffer[Node](seeds._1), n.isLeaf),
      TreeNode(n.parent, ListBuffer[Node](seeds._2), n.isLeaf))

    pickNext(n.children - seeds._1 - seeds._2, result(0), result(1))
    result
  }

  def pickNext(left: ListBuffer[Node], l: TreeNode, r: TreeNode) {
    left match {
      case _ if left.length <= (m - l.children.length) => left foreach { _.parent = l }; l.children ++= left; l.refreshMbr
      case _ if left.length <= (m - r.children.length) => left foreach { _.parent = r }; r.children ++= left; r.refreshMbr
      case _ => {
        def diff(n: Node, ll: Node, rr: Node) = Math.abs(n.mbr.unionBy(ll.mbr).area - n.mbr.unionBy(rr.mbr).area)
        val next = left maxBy (diff((_: Node), l, r))
        List(l, r) minBy ((_: TreeNode).mbr.enlargedBy(next.mbr)) addChild next
        left -= next
        pickNext(left, l, r)
      }
    }
  }

  def PickSeeds(l: ListBuffer[Node]) = {
    def expanded(nodes: (Node, Node)) = nodes._1.mbr.enlargedBy(nodes._2.mbr) - nodes._2.mbr.area
    (l flatMap { a => l map { (a, _) } }) maxBy (expanded(_: (Node, Node)))
  }

  /**
   * Find the entries which overlap with the particular MBR
   */
  def findLeaf(mbr: Mbr): List[EntryNode] = {
    def _findLeaf(node: Node, mbr: Mbr): List[EntryNode] =
      node match {
        case e: EntryNode => if (mbr isOverlap e.mbr) List(e) else List[EntryNode]()
        case t: TreeNode => t.children
          .filter { n => n.mbr isOverlap mbr }
          .foldLeft(List[EntryNode]()) { (l, n) => l union _findLeaf(n, mbr) }
      }
    _findLeaf(root, mbr)
  }

  /**
   * Delete the particular node
   */
  def delete(n: Node) {
    n.parent.children -= n
    condenseTree(n.parent)
  }

  /**
   * After a node is deleted, we should check if its parent node holds less than m children.
   * If so, the tree should be condensed. We should propagate the condensing operation upward
   * till the capacity criteria are satisfied at every layer.
   */
  def condenseTree(n: TreeNode) {
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
        (n.parent.children minBy ((_: Node).mbr.enlargedBy(o.mbr))).asInstanceOf[TreeNode] addChild o
        adjustTree(o.parent)
      }
      condenseTree(n.parent)
    }
  }

  private def lowerTreeWhenRootHasOnlyOneChild(n: TreeNode) {
    if (n.eq(root) && n.children.length == 1 && n.children(0).isInstanceOf[TreeNode]) {
      root = n.children(0).asInstanceOf[TreeNode]
      n.children(0).parent = null
    }
  }
}