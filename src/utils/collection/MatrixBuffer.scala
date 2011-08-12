package utils.collection
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import java.util.NoSuchElementException
import scala.collection.generic._
import scala.collection.mutable.BufferLike
import scala.collection.mutable.IndexedSeqOptimized
import scala.collection.mutable.Builder
import scala.collection.mutable.ResizableArray

object matrixMath {
//  Confidence in correctness: High.
//  Reason: Tested multiple times
  def vp[T <: Seq[Double]](v1: T, v2: T): T = {
    v1.zip(v2).map(x => x._1 + x._2).asInstanceOf[T]
  }

//  Confidence in correctness: High.
//  Reason:Tested multiple times
  def vp[T <: Seq[Double]](v1: T, v2: Double): T = {
    v1.map(x => x + v2).asInstanceOf[T]
  }
}

class ExpandingArray[T](lengthIn: Int, defaultValue: T = null.asInstanceOf[T])  extends ArrayBuffer[T](lengthIn)
with GenericTraversableTemplate[T, ExpandingArray]
     with BufferLike[T, ExpandingArray[T]]
     with IndexedSeqOptimized[T, ExpandingArray[T]]
     with Builder[T, ExpandingArray[T]]
     with ResizableArray[T] {
  override def companion: GenericCompanion[ExpandingArray] = ExpandingArray
  override def result: ExpandingArray[T] = this
  def this(v: Seq[T]) = {this(v.length);  ++=(v)}

//  Confidence in correctness: High.
//  Reason:Tested multiple times
  def addAt(idx: Int, diff: T)(implicit num: Numeric[T]) = {
    padTill(idx+1); update(idx, num.plus(apply(idx), diff))
  }

//  Confidence in correctness: High.
//  Reason:Tested multiple times
  def padTill(len: Int, value: T) =
    {(length to len-1).foreach((x) => +=(value))}
//  Confidence in correctness: High.
//  Reason:Tested multiple times
  def padTill(len: Int):Unit = padTill(len, defaultValue)

//  Confidence in correctness: High.
//  Reason:Tested multiple times
  override def update(index: Int, value: T) = {padTill(index + 1); super.update(index, value)}
}
object ExpandingArray extends SeqFactory[ExpandingArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ExpandingArray[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, ExpandingArray[A]] = new ExpandingArray[A](16)
}

object collectionsTest{
  def vpTest(){
    var x = new ExpandingArray[Double](4)
    x ++= List(1, 2, 3, 4)
    var y = new ExpandingArray[Double](4)
    y ++= List(1.1, 2, 3, 4)
    y(9) = 9
    println(y)
    var z = matrixMath.vp(x,y)
    println(matrixMath.vp(z, 90))
  }
}

abstract class MatrixBuffer[T, X](rowsIn: Int){
  var matrix= new ExpandingArray[X](rowsIn)

//  State variable indicating maximum size of any row.
//  Confidence in correctness: High 
//  Reason: Proved correct.
  var numCols = 0

  def getEmptyRow: X

//  Confidence in correctness: High.
//  Reason: Proved correct, well tested.
  def padRows(numRowsIn: Int) =
    (length to numRowsIn-1).foreach((x) => matrix += (getEmptyRow))

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def length = matrix.length
  def apply(row: Int, col: Int): T
  def update(row: Int, col: Int, value: T)
  
//  Confidence in correctness: High.
//  Reason: Proved correct.
  override def toString(): String = matrix.toString
}

class MatrixBufferDense[T] (rowsIn: Int, colsIn: Int, defaultValue: T = null.asInstanceOf[T]) extends MatrixBuffer[T, ExpandingArray[T]](rowsIn){

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def apply(row: Int, col: Int): T = {
    matrix(row).padTill(numCols, defaultValue)
    matrix(row)(col)
  }

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def apply(row: Int): ExpandingArray[T] = matrix(row)

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def getEmptyRow = new ExpandingArray[T](numCols, defaultValue)

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def padAllRows = matrix.foreach(_.padTill(numCols))

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def expandBuffer(row: Int, col: Int) = {
    padRows(row+1)
    numCols = List(numCols, col+1).max
    matrix(row).padTill(numCols, defaultValue)
  }

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def update(row: Int, col: Int, value: T) = {
    expandBuffer(row, col)
    matrix(row)(col) = value
  }

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def increment(row: Int, col: Int)(implicit numeric: Numeric[T]) = {
    expandBuffer(row, col)
    matrix(row)(col) = numeric.plus(apply(row, col), 1.asInstanceOf[T])
  }
}

//Each row is a HashMap where keys are stored in a trie/ prefix tree. So excessive memory is not wasted in storing sparse data.
class MatrixBufferRowSparse[T] (rowsIn: Int, defaultValue: T = null.asInstanceOf[T]) extends MatrixBuffer[T, HashMap[Int, T]](rowsIn){

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def apply(row: Int, col: Int): T = if(col >= numCols) throw new NoSuchElementException(""+col)
    else return matrix(row).getOrElse(col, defaultValue)

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def apply(row: Int): HashMap[Int, T] = matrix(row)

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def getEmptyRow = new HashMap[Int, T]()

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def expandBuffer(row: Int, col: Int) = {
    padRows(row+1)
    numCols = List(numCols, col+1).max
  }

//  Confidence in correctness: High.
//  Reason: Proved correct.
  def update(row: Int, col: Int, value: T) = {
    expandBuffer(row, col)
    if(value == 0) matrix(row) = matrix(row) - col
    else matrix(row) = matrix(row) + (col -> value)
  }
  
//  Confidence in correctness: High.
//  Reason: Proved correct.
  def increment(row: Int, col: Int)(implicit numeric: Numeric[T]) = {
    expandBuffer(row, col)
    matrix(row) = matrix(row) + (col -> numeric.plus(apply(row, col), 1.asInstanceOf[T]))
  }
}

