### Runtime parameters:

-   The object Main is the point of entry for the program. It reads various parameters for the tagging experiment, such as the language, corpus, tagging algorithm to be used etc.. from a properties file whose path is passed as an argument to the program.
-   An <span style="font-weight: bold;">example properties file</span> called partofspeechtagger/runtimeSettings.properties is included. This is the file used by default, and you may use this as a template.

### Directory structure for data files:

-   Please set DATA\_DIR = “/home/vvasuki/posTagging/data/” as appropriate in your runtimeSettings.properties file.
-   \# The directory wherein the training, test and wiktionary files are stored.
    \# 1. Please get all data files from <http://ilk.uvt.nl/conll/free_data.html> and extract them under DATA\_DIR.
    \# 1a. Commands for doing this are given in <http://sourceforge.net/apps/mediawiki/opennlp/index.php?title=Conll06#Extract_data>.
    \# 2. Get all files from <http://nlp-s11.utcompling.com/assignments/hmm-tagging/ALL_DATA_TAGGING.tgz?attredirects=0> .
    \# 2a. Extract and rename files obtained in \[2\] so that the following directory structure results:
    \#  ./ic/test: ictest
    \#  ./ic/train: icraw ictrain
    \#  ./en/test: entest
    \#  ./en/train: enraw entrain entrain10k entrain25k entrain4k
    \# 3. Download and extract the wiktionary file it: [http://toolserver.org/~enwikt/definitions/enwikt-defs-latest-all.tsv.gz]
    \# 4. Download and extract the universal tag sets: <http://code.google.com/p/universal-pos-tags/>.

### Compilation:

<ul>
<li>
Source directories:

</li>
-   The [repository] provides a source directory called src.

<!-- -->

-   Include junto source code as a source directory. (We will alter Vertex.java . )

<li>
Jar files in the class path:

</li>
-   The jar files required by junto, the open-nlp jar files are required.

<li>
Build everything.

</li>
-   Note that the directory tree where the \*.class files are stored should also contain a copy of \*.properties files found in the source directories.

</ul>

### Extra function added to Vertex.java in junto:

//  Return the estimated label with the highest score.
//  Confidence in correctness: Low.
//  Reason: proved correct, but estimated\_labels\_ seems to have NaN scores.
//  Assumption: estimated\_labels\_ has atleast one tag with score &gt;= 0.
//  Claim: bestLabel is the label with the maximal score.
  public String getEstimatedLabelBest(){
     String\[\] labels = estimated\_labels\_.keys(new String\[1\]);
     if(labels.length == 0) throw new NullPointerException();
     String bestLabel = “”;
     double bestScore = -1;
     for(int i=0; i&lt; labels.length; i++){
        if(labels\[i\].contains(“\_DUMMY\_”)) continue;
         double score = estimated\_labels\_.get(labels\[i\]);
         if(score &gt; bestScore) {
             bestScore = score;
             bestLabel = labels\[i\];
         }
     }
     return bestLabel;
 }

### Setting the classpath etc:

-   The jar files required by junto, the open-nlp jar files are required.
-   In addition, the classpath should include the directory containing the compiled junto packages.
-   Note that the directory tree where the project’s .class files are stored should also contain a copy of \*.properties files found in the source directories.

### Files/ functions to be fixed:

LabelPropagationTagger.scala, which implements the “tagging by label propagation” idea, has methods to use training and test data. Some documentation is included. Especially, the lines after JuntoRunner(graph, 1.0, .01, .01, 50, false) in the method predict() yield unanticipated output.
