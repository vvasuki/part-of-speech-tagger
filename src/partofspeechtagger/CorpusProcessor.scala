package partofspeechtagger

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import utils.collection.BijectiveHashMap
import utils.io.TextTableParser
import java.util.NoSuchElementException

class CorpusProcessor(language: String, corpus: String, taggerType: String = "WordTagProbabilities"){
  var numTestTokensKnown = 0
  var numTestTokensNovel = 0
  var correctTaggingsKnown = 0
  var correctTaggingsNovel = 0

  val tagIntMap = new BijectiveHashMap[String, Int]
  val wordIntMap = new BijectiveHashMap[String, Int]

  val DATA_DIR = Main.DATA_DIR
  val TEST_DIR = "test"
  val TRAINING_DIR = "train"
  val WIKTIONARY = DATA_DIR+"TEMP-S20110618.tsv"

  var unmappedTags = 0
  val LANGUAGE_CODE_MAP = getClass.getResource("languageCodes.properties").getPath
  val TAG_MAP_DIR = DATA_DIR+"universal_pos_tags.1.02/"

  var sentenceSeparatorTag = "###"
  var sentenceSeparatorWord = "###"
  val sentenceSepWordId = getWordId(sentenceSeparatorWord)
  val sentenceSepTagId = getTagId(sentenceSeparatorTag, sentenceSeparatorWord)



//  Confidence in correctness: High.
//  Reason: Proved correct.
  var tagger: Tagger = new WordTagProbabilities()
  taggerType match {
    case "HMM" => tagger = new HMM(sentenceSepTagId, sentenceSepWordId)
    case "LabelPropogationTagger" => tagger = new LabelPropogationTagger(sentenceSepTagId, sentenceSepWordId)
    case _ => tagger = new WordTagProbabilities()
  }

  

  var languageCode = ""
//X - other: foreign words, typos, abbreviations
//ADP - adpositions (prepositions and postpositions)
//. - punctuation
  val tagsUniversal = List("VERB", "NOUN", "PRON", "ADJ", "ADV", "ADP", "CONJ", "DET", "NUM", "PRT", "X", ".")
  val tagMap = new HashMap[String, String]()
  val wiktionaryTagMap = new HashMap[String, String]()


//  Confidence in correctness: High.
//  Reason: Well tested.
  if(Main.bUniversalTags)  {
    // Deduce language code
    var parser = new TextTableParser(file = LANGUAGE_CODE_MAP, separator = ' ', x =>x.length >= 2, lineMapFn = null)
    var iter = parser.getRowIterator.filter((x) => x(1).equalsIgnoreCase(language))
    languageCode = (iter.next())(0)
    println(languageCode)

    // Populate the tagMap by reading the appropriate file.
    parser = new TextTableParser(file = TAG_MAP_DIR + languageCode + "-" + corpus + ".map", filterFnIn = (x =>x.length >= 2), lineMapFn = (x => x.map(_.toUpper)))
    parser.getRowIterator.foreach(x => tagMap(x(0)) = x(1))
    tagMap.values.foreach(x => tagMap(x) = x)

    //  Add the universal tags themselves to the map.
    tagsUniversal.foreach(x => tagMap(x) = x)
//    print(tagMap)
  }

  if(Main.bWiktionary) processFile(WIKTIONARY)
  if(Main.bUseTrainingData) processFile(TRAINING_DIR)

  /*
   * Add mapping for a tag to the tag map.
   * Assumption: There is no pre-existing mapping.
   * Arguments: tagIn: the tag to be added.
   *  word: A word corresponding to the tag; used for printing a helpful message.
   */
//  Confidence in correctness: High.
//  Reason: Well tested.
  def updateTagMap(tagIn: String, word: String): Unit= {
    var tag = tagIn.map(_.toUpper)
    if(tagMap.contains(tag)) throw new IllegalArgumentException("Mapping already exists")
    var iter = tagsUniversal.filter(x => !(x.equals("X") || x.equals(".")))
    for(tagUniversal <- iter) {
      if(tag.indexOf(tagUniversal) != -1) {tagMap(tag) = tagUniversal; return}
    }
//    Begin special cases
//ADP - adpositions (prepositions and postpositions)
    if(tag.indexOf("POSITION") != -1) {tagMap(tag) = "ADP"; return}
//X - other: foreign words, typos, abbreviations
    if(tag.indexOf("ABBREV") != -1 || tag.indexOf("FOREIGN") != -1 || tag.indexOf("ACRONYM") != -1 || tag.indexOf("INITIAL") != -1) {tagMap(tag) = "X"; return}
//. - punctuation
    if(tag.indexOf("PUNCTUAT") != -1) {tagMap(tag) = "X"; return}
    if(tag.indexOf("PARTICLE") != -1) {tagMap(tag) = "PRT"; return}
    if(tag.indexOf("ARTICLE") != -1) {tagMap(tag) = "DET"; return}
    unmappedTags = unmappedTags+1
    println("unmapped tag "+tag + " :word "+ word);
    tagMap(tag) = tag;
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def getTagId(tagIn: String, word: String): Int = {
    /*
     * Map a tag, update the mapping if necessary.
     */
//  Confidence in correctness: High.
//  Reason: Proved.
    def getMappedTag(tagIn1: String, word: String): String = {
      var tag = tagIn1
      try{tag = tagMap(tag);}
      catch{case e => updateTagMap(tag, word)}
      return tag
    }

    var tag = tagIn
    if(Main.bUniversalTags)
      tag = getMappedTag(tag, word)

    if(!tagIntMap.contains(tag))
      tagIntMap.put(tag, tagIntMap.size)
    tagIntMap(tag)
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def getWordId(word: String): Int = {
    if(!wordIntMap.contains(word))
      wordIntMap.put(word, wordIntMap.size)
    wordIntMap(word)
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def getAccuracy() = {
    processFile(TEST_DIR)
    var correctTaggings = correctTaggingsKnown + correctTaggingsNovel
    var numTestTokens = numTestTokensKnown + numTestTokensNovel
    var accuracy = correctTaggings/ numTestTokens.toDouble
    var accuracyKnown = correctTaggingsKnown/ numTestTokensKnown.toDouble
    var accuracyNovel = correctTaggingsNovel/ numTestTokensNovel.toDouble

    printf("Accuracy: %.2f, (Known: %.2f, Novel: %.2f)\n", accuracy, accuracyKnown, accuracyNovel)
    printf("Non training words: %d, %.2f\n", numTestTokensNovel, numTestTokensNovel/numTestTokens.toDouble)
  }

  def test = {
    println(language + ' ' + corpus);
    getAccuracy()
    println("Most frequent tag overall: "+ tagger.bestTagsOverall)
    if(Main.bUniversalTags) println(unmappedTags + " unmapped tags.")
  }

//  Confidence in correctness: High.
//  Reason: Well tested.
  def processFile(mode: String) = {
    def getFileName(fileType: String): String = {
      var languageCorpusString = language;
      if(!corpus.equals("")) languageCorpusString = languageCorpusString + '/' + corpus
      var file = DATA_DIR + languageCorpusString + '/'+ fileType+'/' + languageCorpusString.replace("/", "_") + '_'+ fileType
      if(!corpus.equals("")) file = file + ".conll"
      else file = file.replace("_", "")
      file
    }

    if(mode.equals(TEST_DIR)) {
      numTestTokensKnown = 0
      numTestTokensNovel = 0
      correctTaggingsKnown = 0
      correctTaggingsNovel = 0
    }

    val file = getFileName(mode)
//    println(file)

    var wordField = 1
    var tagField = 3;
    var sep = '\t'
    if(language.equals("danish")) tagField = 4
    if(corpus.equals("")) {
      wordField = 0; tagField = 1; sep = '/'
    }
    if(mode.equals(WIKTIONARY))tagField = 2


    var filterFn = ((x:Array[String]) => (x.length >= tagField+1))
    if(mode.equals(WIKTIONARY))
      filterFn = ((x:Array[String]) => ((x.length >= tagField+1) && x(0).equalsIgnoreCase(language)))

    var newSentenceLine = sentenceSeparatorWord;
    for(i <- 1 to tagField) newSentenceLine = newSentenceLine + sep + sentenceSeparatorTag
    var lineMap = (x:String)=> {var y = x.trim;
                                if(y.isEmpty()) y= newSentenceLine;
                                y.map(_.toUpper)}

    val parser = new TextTableParser(file = file, separator = sep, filterFnIn = filterFn, lineMapFn = lineMap)
    val iter = parser.getFieldIterator(wordField, tagField).map(x => Array(getWordId(x(0)), getTagId(x(1), x(0))))
    if(!mode.equals(TEST_DIR)) {
      tagger.train(iter)
    }
    else {
      val testData = new ArrayBuffer[Array[Int]](10000)
      iter.copyToBuffer(testData)
      println(testData.length)
      val tags = tagger.predict(testData)
      for {i <- tags.indices.iterator
        if(testData(i)(0) != sentenceSepWordId)
     }{
        var wordId = testData(i)(0)
        var tagId = testData(i)(1)
        var tagBest = tags(i)._1
        var bNovelToken = tags(i)._2
        var bCorrect = tagBest.equals(tagId)
//        if(!bCorrect)
//            println(wordId + " " + tagIntMap.getKey(tagBest) + " " + tagIntMap.getKey(tagId))
        if(!bNovelToken) {
          if(bCorrect) correctTaggingsKnown  = correctTaggingsKnown  + 1
          numTestTokensKnown = numTestTokensKnown + 1;
        }
        else {
          if(bCorrect) correctTaggingsNovel  = correctTaggingsNovel  + 1
          numTestTokensNovel = numTestTokensNovel + 1;
        }
      }
    }

  }

}
