package cn.edu.neu.chiewen.cknn.vtree

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, RandomAccessFile}
import cn.edu.neu.chiewen.cknn.algorithms.Util.DoubleTuple2Mbr
import cn.edu.neu.chiewen.cknn.site.NodeDisk.NodeDb2Mbr
import cn.edu.neu.chiewen.cknn.site.{NeighboredSiteDisk, NeighboredSiteMemory, NodeDisk}
import cn.edu.neu.chiewen.rtree.core.{EntryNode, Node, TreeNode}
import scala.collection.mutable
import scala.language.implicitConversions

class VTreeIO(val dataFile: String, val indexFile: String) {
  lazy val data = new RandomAccessFile(dataFile, "r")
  lazy val indexStream = new ObjectInputStream(new BufferedInputStream(new FileInputStream(indexFile)))
  lazy val index = indexStream.readObject().asInstanceOf[Map[Long, Long]]

  def root = {
    val pos = data.getFilePointer
    data.seek(0)
    val r = NodeDisk.readNodeDisk(data)
    data.seek(pos)
    r
  }

  def get(id: Long) = {
    data.seek(index(id))
    NodeDisk.readNodeDisk(data)
  }

  /**
   * Of the same skeleton with the "knn" method of the VTree class
   */
  def knn(k: Int, p: (Double, Double)) = {
    var c = 1

    val mbrs = mutable.PriorityQueue.empty(Ordering.fromLessThan((a: NodeDisk, b: NodeDisk) =>
      p.distanceToNS(a) > p.distanceToNS(b)))
    mbrs.enqueue(root)

    var result: List[NeighboredSiteDisk] = Nil
    while (result.length < k && mbrs.nonEmpty)
      mbrs.dequeue() match {
        case t if t.isEntry =>
          result ::= t; c += 1
        case t => for (c <- t.children) mbrs.enqueue(get(c)); c += t.children.length
      }
    (result.reverse, c)
  }

}

object VTreeIO {
  implicit def Node2NodeDb(n: Node): NodeDisk =
    new NodeDisk(n.mbr.mid, n.mbr.coordinates(0), n.mbr.coordinates(1), n match {
      case p: EntryNode => p.mbr.asInstanceOf[NeighboredSiteMemory].neighbors.toList.map(n => new NeighboredSiteDisk(n.mid, n.id, n.position))
      case _ => List[NeighboredSiteDisk]()
    }, n match {
      case p: TreeNode => p.children.map(_.mbr.mid).toList
      case _ => List[Long]()
    }, n match {
      case p: EntryNode => p.mbr.asInstanceOf[NeighboredSiteMemory].id
      case _ => -1
    })

  def write(tree: VTree, dataFile: String, indexFile: String, objectFile: String) {
    val data = new RandomAccessFile(dataFile, "rw")
    val index = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(indexFile)))
    val oos = new ObjectOutputStream(new FileOutputStream(objectFile))

    var ids = Map[Long, Long]()

    def _write(n: Node) {
      ids += NodeDisk.writeNodeDisk(data, n)
      n match {
        case p: EntryNode =>
        case p: TreeNode => p.children foreach _write
      }
    }

    _write(tree.getRoot)
    index.writeObject(ids)

    oos.writeObject(tree)

    data.close()
    index.close()
    oos.close()
  }
}
