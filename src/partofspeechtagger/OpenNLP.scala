package partofspeechtagger

import opennlp.tools.postag._
import opennlp.tools.util._
import opennlp.tools.util.model._
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Stream

class OpenNLP(sentenceSepTag :Int, sentenceSepWord: Int, languageCode: String) extends Tagger{
  //  Warning: The implementation of reset does not behave as expected.
  //  This is because of the difficulty in constructing a stream from a scala iterator.
  //  This is by design: we think that reset() will not be called.
  class TaggedSentenceStream(wordTagIter: Iterator[Array[Int]]) extends ObjectStream[POSSample] {
//    Supposed to equal one more than the maximum wordId found in wordTagIter so far by read().
//    Confidence: High
//    Reason: read() proved correct.
    var numWords = 0

//  Confidence: Low.
//  Reason: Deliberately not well designed.
//  We assume that the defined funcitonality is not required.
    def close() = {println("TaggedSentenceStream.close")}

//  Confidence: Low.
//  Reason: Deliberately not well designed.
//  We assume that the defined funcitonality is not required.
//    @throws(classOf[java.io.IOException])
    def reset() = {throw new java.io.IOException("Undefined operation.")}

//  Returns:
//    the next object or null to signal that the stream is exhausted.
//  Side-effect:
//    update numWords appropriately.
//  Confidence in correctness: High.
//  Reason: Proved correct.
    def read : POSSample = {
      if(wordTagIter.isEmpty) return null

      var words = new ArrayBuffer[String](20)
      var tags = new ArrayBuffer[String](20)

      def addWord(wordId: Int) = {
        words += wordId.toString;
        numWords = List(wordId + 1, numWords).max
      }

      wordTagIter.takeWhile(x => x(1) != sentenceSepTag).foreach(x => {
          addWord(x(0)); tags += x(1).toString})
//      println(words)
//      println(tags)
      new POSSample(words.toArray, tags.toArray)
    }
  }


  var model: POSModel = null
  var numWords  = 0
  
// Computation:
//  Update numWords.
//  Train the POSModel.
//  Confidence in correctness: High.
//  Reason: Proved Correct.
//  Claims:
//    numWords is updated correctly.
//    model is set appropriately.
  def train(wordTagIter: Iterator[Array[Int]]) = {
    var sentenceStream = new TaggedSentenceStream(wordTagIter)
    val numIters = 15
    model = POSTaggerME.train(languageCode.map(_.toLower), sentenceStream, ModelType.MAXENT,
      null, null, 100, numIters);
    numWords = sentenceStream.numWords
  }
  
//  Confidence in correctness: High.
//  Reason: Proved correct.
  def predict(testData: ArrayBuffer[Array[Int]]): ArrayBuffer[(Int, Boolean)] = {
    val numTokens = testData.length
    var finalTags = new ArrayBuffer[(Int, Boolean)](numTokens)
    val tagger = new POSTaggerME(model);
    val tokens = testData.map(x => x(0).toString).toArray
    val tags = tagger.tag(tokens)
    (0 to numTokens-1).foreach(x => finalTags += ((tags(x).toInt, testData(x)(0) >= numWords)))
    finalTags
  }

}
