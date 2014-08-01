package cn.edu.neu.chiewen.cknn

trait ResultKeeper {
  var children = List[ResultKeeper]()

  def addResultChildren(c: ResultKeeper) {
    children ::= c
  }

  def showResult(s: String)
}

class Result extends ResultKeeper {
  val resultMap = scala.collection.mutable.Map[String, String]()

  def addResult(s: String, r: String) {
    if (!resultMap.contains(s)) resultMap += s -> r
    resultMap(s) = r
  }

  def showResult(s: String) {
    if (resultMap.contains(s)) print(resultMap(s) + "\t")
  }
}