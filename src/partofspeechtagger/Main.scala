package partofspeechtagger
import utils.collection._

object Main {
  var bUniversalTags = false
  var bUseTrainingData = true; var bWiktionary = false;
  if(bWiktionary) bUniversalTags = true;
  else bUseTrainingData = true;
  
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    println("Using universal tags? "+ bUniversalTags)
    println("Using wiktionary? "+ bWiktionary)
    println("Using training data? "+ bUseTrainingData)
//    collectionsTest.vpTest()
    var wtp = new CorpusProcessor("ic", "", "LabelPropogationTagger").test
//    var wtp = new CorpusProcessor("en", "", "HMM").test
//    wtp = new CorpusProcessor("portuguese","bosque").test
//    wtp = new CorpusProcessor("danish","ddt").test;
//    wtp = new CorpusProcessor("dutch","alpino").test;
//    wtp = new CorpusProcessor("swedish","talbanken05").test;
  }

}
