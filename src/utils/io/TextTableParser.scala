package utils.io
import scala.io.Source

class TextTableParser(file: String, separator: Char = '\t', filterFnIn: Array[String] => Boolean = (x =>x.length >=1), lineMapFn : String => String = null){
  val src = Source.fromFile(file)

  def getRowIterator: Iterator[Array[String]] = {
    var lines = src.getLines()
    if(lineMapFn != null) lines = lines.map(lineMapFn)
    lines.map(_.split(separator)).filter(filterFnIn)
  }

  def getFieldIterator(fieldId1: Int, fieldId2: Int): Iterator[Array[String]] = {
    getRowIterator.map(x => Array(x(fieldId1), x(fieldId2)))
  }

  override protected def finalize : Unit = { src.close(); super.finalize}
}

