package cn.edu.neu.chiewen.cknn.site

import java.io.RandomAccessFile

import cn.edu.neu.chiewen.cknn.server.Server
import cn.edu.neu.chiewen.rtree.core.Mbr

class NodeDisk(val mid: Long, val p1: (Double, Double), val p2: (Double, Double),
               val neighbors: List[NeighboredSiteDisk], val children: List[Long], val id: Int) extends NeighboredSite {
  val position = (p1._1, p2._1)

  def isEntry = !neighbors.isEmpty

  def getNeighbors(implicit server: Server = null) = neighbors

  override def hashCode = (41 * (41 + mid)).toInt

  override def equals(o: Any) = o match {
    case that: NodeDisk => NodeDisk.this.mid == that.mid
    case _ => false
  }
}

object NodeDisk {

  import scala.language.implicitConversions

  implicit def NodeDb2Mbr(n: NodeDisk) = new Mbr(List(n.p1, n.p2), n.mid)

  implicit def NodeDb2DoubleTuple(n: NodeDisk) = n.position

  implicit def NodeDb2NeighboredSiteDb(n: NodeDisk) = new NeighboredSiteDisk(n.mid, n.id, n.position)

  private var rc = 0;

  def reset {
    rc = 0
  }

  def readCount = rc

  def readNodeDisk(raf: RandomAccessFile): NodeDisk = {
    rc = rc + 1
    val id = raf.readLong()
    val p1 = (raf.readDouble(), raf.readDouble())
    val p2 = (raf.readDouble(), raf.readDouble())
    val r = for (i <- (1 to raf.readInt())) yield new NeighboredSiteDisk(raf.readLong, raf.readInt, (raf.readDouble(), raf.readDouble()))
    val c = for (i <- (1 to raf.readInt())) yield raf.readLong
    new NodeDisk(id, p1, p2, r.toList, c.toList, raf.readInt())
  }

  def writeNodeDisk(raf: RandomAccessFile, node: NodeDisk) = {
    val pos = raf.getFilePointer()
    raf.writeLong(node.mid)
    raf.writeDouble(node.p1._1)
    raf.writeDouble(node.p1._2)
    raf.writeDouble(node.p2._1)
    raf.writeDouble(node.p2._2)
    raf.writeInt(node.neighbors.length)
    for (i <- node.neighbors) yield {
      raf.writeLong(i.mid)
      raf.writeInt(i.id)
      raf.writeDouble(i.position._1)
      raf.writeDouble(i.position._2)
    }
    raf.writeInt(node.children.length)
    for (i <- node.children) yield raf.writeLong(i)
    raf.writeInt(node.id)

    (node.mid -> pos)
  }
}