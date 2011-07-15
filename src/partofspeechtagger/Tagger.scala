package partofspeechtagger

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedList
import java.util.NoSuchElementException


trait Tagger {
  val TAGNUM_IN = 25
  val WORDNUM_IN = 3000
  var bestTagsOverall = new LinkedList[Int]()
  def train(iter: Iterator[Array[Int]])
  def predict(testData: ArrayBuffer[Array[Int]]): ArrayBuffer[(Int, Boolean)]
}

class WordTagProbabilities() extends Tagger {

  val wordTagFrequencies = new HashMap[Int, (Array[Int], LinkedList[Int])]()
  var tagFrequenciesOverall = new Array[Int](TAGNUM_IN)


//  Confidence in correctness: High.
//  Reason: Well tested.
  def getBestTagsFromArray(tf: Array[Int]):LinkedList[Int] = {
    new LinkedList[Int]() ++ tf.indices.filter((x) => (tf(x) == tf.max))
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def train(iter: Iterator[Array[Int]]) = {
    for(fields <- iter) updateFrequency(fields(0), fields(1))
    bestTagsOverall = getBestTagsFromArray(tagFrequenciesOverall)
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def getBestTags(word: Int):LinkedList[Int] = {
    if(!wordTagFrequencies.contains(word))
          return  bestTagsOverall
    var (tagFrequencies, tags) = wordTagFrequencies(word)

    var bestTags = tags
    if(bestTags == null) {
      bestTags = getBestTagsFromArray(tagFrequencies)
      wordTagFrequencies(word) = (tagFrequencies, bestTags)
    }
    return bestTags

  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def predict(testData: ArrayBuffer[Array[Int]]): ArrayBuffer[(Int, Boolean)] = {
    var tags = new ArrayBuffer[(Int, Boolean)](testData.length)
    tags = tags.padTo(testData.length, (0, false))

    testData.indices.foreach(i => tags(i) = (getBestTags(testData(i)(0)).head, !wordTagFrequencies.contains(testData(i)(0))))
    tags
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def updateFrequency(word: Int, tag: Int) = {
    var tagFrequenciesWord = new Array[Int](TAGNUM_IN)
    var bestTags = new LinkedList[Int]()
    if(wordTagFrequencies.contains(word)){
      val x= wordTagFrequencies(word);
      tagFrequenciesWord = x._1; bestTags = x._2;
    }

    tagFrequenciesWord(tag) = tagFrequenciesWord(tag)+1
    wordTagFrequencies(word) = (tagFrequenciesWord, null)
    tagFrequenciesOverall(tag) = tagFrequenciesOverall(tag)+1
  }

}
