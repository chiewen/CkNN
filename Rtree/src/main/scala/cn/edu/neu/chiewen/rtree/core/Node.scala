package cn.edu.neu.chiewen.rtree.core

import scala.collection.mutable.ListBuffer

/**
 * The node can be a leaf, or an intermediate node.
 */
abstract class Node(var parent: TreeNode, var mbr: Mbr) extends Serializable { def traverse(l: Int)(f: (Mbr, Int) => Unit) }

class TreeNode(p: TreeNode, m: Mbr,
  var children: ListBuffer[Node], val isLeaf: Boolean) extends Node(p, m) with Serializable {
  override def toString = {
    val sb = new StringBuilder("\n[")
    sb.append(mbr + "|")
    (sb /: children) { (sb, n) => sb.append(n.toString()) }
    sb.append("]")
    sb.toString()
  }

  def traverse(l: Int)(f: (Mbr, Int) => Unit) {
    f(m, l)
    children foreach { _.traverse(l + 1)(f) }
  }

  def refreshMbr { mbr = Mbr.union(children map (_.mbr)) }

  def addChild(child: Node) {
    children += child
    child.parent = this
    mbr = mbr.unionBy(child.mbr)
  }

  def overlappedArea(m: Mbr) = (0.0 /: children) { (p, c) => p + c.mbr.overlappedArea(m) }
}

object TreeNode {
  def apply(p: TreeNode, children: ListBuffer[Node], isLeaf: Boolean) = {
    val node = new TreeNode(p, Mbr.union(children map { _.mbr }), children, isLeaf)
    node.children foreach { _.parent = node }
    node.refreshMbr
    node
  }
}

class EntryNode(p: TreeNode, m: Mbr) extends Node(p, m) with Serializable {
  //must override the default toString method, since the default one would call parent.toString 
  //which could lead to loop calling.
  override def toString = mbr.toString()

  def traverse(l: Int)(f: (Mbr, Int) => Unit) = f(m, l)

}