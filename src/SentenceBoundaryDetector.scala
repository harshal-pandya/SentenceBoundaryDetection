import io.Source

class SentenceBoundaryDetector(text: String) {
  /**
   * Sentence boundar marker
   */
  val S = 1
  /**
   * Abbreviation Marker
   */
  val A = 2
  /**
   * Elipses Marker
   */
  val E = 3
  /**
   * Abbreviation at end of sentence marker
   */
  val AS = 4

  val (tokens,tokenStrings) = getTokens(text)
  val TOKEN_MAP = tokens.groupBy(x => x._1.toLowerCase)
  /**
   * Stores the case-insensitive count of each distinct word
   * It counts a word and a word ending in a period as distinct
   */
  val wordCount = tokens.groupBy(x => x._1.toLowerCase).mapValues(_.length)
  val N_words = tokens.length

  /**
   * Tokenizes the text and prepares a list of tokens.<br>
   * A token is a 3-tuple which contains:
   * <ul>
   * <li>The actual token String
   * <li>The start index of the token in text
   * <li>The next token String
   * </ul>
   * @param text The text from the document
   * @return     A list of tokens and a list of token strings
   */
  def getTokens(text:String)={
    val tempText = text.split("""[\r\n]+""").mkString(" ")
    val regex = """\w+'[\.\-\w]+|\w+[\.\-\w]*"""
    val tokenIt = regex.r.findAllIn(tempText)
    val tokenList = regex.r.findAllIn(text).toList
    val mutableList = scala.collection.mutable.ListBuffer[(String,Int,String)]()
    val mutableList1 = scala.collection.mutable.ListBuffer[String]()
    var index = 1
    while(tokenIt.hasNext) {
      var temp = tokenIt.next()
      if(temp.charAt(0).isDigit) {
        temp = isNumeric(temp)
      }
      if (index==tokenList.length)
        mutableList++=Seq((temp,tokenIt.start,""))
      else
        mutableList++=Seq((temp,tokenIt.start,tokenList(index)))

      mutableList1++=Seq(temp)
      index+=1
    }
    (mutableList.toList,mutableList1.toList)
  }

  /**
   * If the input string is a number it replaces the string with<br>
   *   <b>##number##</b> maintaining any trailing period.
   * @param str A string that is a candidate for being a number
   * @return    The string <b>##number##</b> if the str is numeric.<br>
   *            Otherwise, str itself.
   */
  def isNumeric(str:String):String={
    var temp = str
    try{
      if (temp.endsWith(".")){
        temp = temp.substring(0,temp.length-1)
        Some(temp.toDouble)
        "##number##."
      }else{
        Some(temp.toDouble)
        "##number##"
      }
    }
    catch{
      case _ : java.lang.NumberFormatException => str
    }
  }


  /**
   * <p> The type-based classiﬁcation stage employs
   * three characteristic properties of abbreviations:
   * <ol>
   *   <li>Strong collocational dependency: Abbreviations always occur with a ﬁnal period.
   *   <li>Brevity: Abbreviations tend to be short.
   *   <li>Internal periods: Many abbreviations contain additional internal periods.
   * </ol>
   * </p>
   * <p>As these three characteristics do not change for
   * each individual instance of a type, we combine them in
   * a type-based approach to abbreviation detection.
   * </p>
   */
  class TypeBasedClassifier {
    val periodCount = wordCount.filter {
      kv => kv._1.endsWith(".")
    }.foldLeft(0)((acc, kv) => acc + kv._2)
    val N = N_words+ periodCount
    val p = periodCount.asInstanceOf[Double] / N

    val distinctWords = tokens.map(t => {
      if (t._1.endsWith(".")) {
        t._1.substring(0, t._1.length - 1).toLowerCase
      }
      else {
        t._1.toLowerCase
      }
    }).toSet

    val likelihoodRatios = distinctWords.flatMap(w => {
      val i1 = wordCount.getOrElse(w, 0) //count of word without period
      val i2 = wordCount.getOrElse(w + ".", 0) //count of word with period
      if (i2!=0) {
        val n1 = i1+i2
        val n2 = N-i1-i2
        val k1 = i2
        val k2 = periodCount - i2
        val p1 = 0.99
        val p2 = k2/n2.asInstanceOf[Double]
        val fLength = 1/math.exp(w.replaceAll("""\.""","").length)
        val fPeriods = """\.""".r.findAllIn(w).length + 1 //number of internal periods
        Seq(w -> logLikehoodRatio(n1,k1,p1,n2,k2,p2,p)*fLength*fPeriods)
      }
      else Nil
    })

    def apply() = {
      likelihoodRatios.flatMap(kv=> {
        if(kv._2>=0.3){
          TOKEN_MAP.get(kv._1+".").get.map(token=>{
            (token._1,token._2,token._3,A)
          })
        }else{
          TOKEN_MAP.get(kv._1+".").get.map(token=>{
            (token._1,token._2,token._3,S)
          })
        }
      }).toList
    }

  }

  class TokenBasedClassifier(tokenList:List[(String, Int, String, Int)]) {
    val UNDECIDED = 0
    val SENTENCE_BOUNDARY = 1
    val NO_SENTENCE_BOUNDARY = 2
    //def initDs(tokenList:List[(String, Int, String, Int)])
    val sCount = tokenList.filter(_._4==S).map(_._3).groupBy(x=>x).mapValues(_.length)
    val aCount = tokenList.filter(_._4==A).map(_._3).groupBy(x=>x).mapValues(_.length)
    val tokenStringsCount = tokenStrings.groupBy(x=>x).mapValues(_.length)
    val internalWords = tokenStringsCount.map(kv=>{
      kv._1 -> (kv._2-sCount.getOrElse(kv._1,0)-aCount.getOrElse(kv._1,0))
    })
    def decideOrthographic(token:String):Int ={
      if(token.charAt(0).isUpper){
        if(tokenStringsCount.getOrElse(token.toLowerCase,0)!=0){
          if(internalWords.getOrElse(token,0)==0){
            SENTENCE_BOUNDARY
          }else{
            UNDECIDED
          }
        }else{
          UNDECIDED
        }
      }else{
        if(tokenStringsCount.getOrElse(token.charAt(0).toUpper+token.substring(1),0)!=0 || sCount.getOrElse(token,0)==0){
          NO_SENTENCE_BOUNDARY
        }else{
          UNDECIDED
        }
      }
    }

    def collocationHeuristic(tokenList:List[(String, Int, String, Int)])={
      tokenList.flatMap(token=> {
        if (token._4==A){
          val token1 = if(token._1.endsWith("."))token._1.substring(0,token._1.length-1) else token._1
          val token2 = if(token._3.endsWith("."))token._3.substring(0,token._3.length-1) else token._3
          //println(token._1 +":"+token1)
          //println(token._3 +":"+token2)
          val i2 = wordCount.getOrElse(token2.toLowerCase,0)+wordCount.getOrElse(token2.toLowerCase+".",0)
          val k1 = tokens.filter(t=> t._1.equals(token._1) && t._3.equals(token._3)).length
          //println(k1)
          val k2 = i2 - k1
          val n1 = wordCount.getOrElse(token1.toLowerCase,0)+wordCount.getOrElse(token1.toLowerCase+".",0)
          val n2 = N_words - n1
          val p1 = k1.asInstanceOf[Double]/n1
          val p2 = k2.asInstanceOf[Double]/n2
          val p = i2.asInstanceOf[Double]/N_words
          //println(n1+":"+k1+":"+p1+":"+n2+":"+k2+":"+p2+":"+p)
          if (p1>p){
            val likelihoodRatio = logLikehoodRatio(n1,k1,p1,n2,k2,p2,p)
            //println(token._1+":"+token._3+":"+likelihoodRatio)
            if (likelihoodRatio>=7.88){
              //Seq((token._1,token.,token._3)->likelihoodRatio)
              Seq(token)
            }else{
              Nil
            }
          }else{
            Nil
          }
        }else Nil
      })
    }
    def freqSentenceStarterHeuristic(tokenList:List[(String, Int, String, Int)])={
      val N_S = tokenList.filter(_._4==S).length

      tokenList.flatMap(token=>{
        val w = if(token._3.endsWith(".")) token._3.substring(0,token._3.length-1) else token._3
        val i2 = wordCount.getOrElse(w.toLowerCase,0)+wordCount.getOrElse(w.toLowerCase+".",0)
        val k1 = tokenList.filter(t=> t._3.equals(token._3) && t._4==S).length
        val k2 = i2 - k1
        val p1 = k1.asInstanceOf[Double]/N_S
        val p2 = k2.asInstanceOf[Double]/N_words
        val p = i2.asInstanceOf[Double]/(N_S+N_words)
        //println(N_S+":"+k1+":"+p1+":"+N_words+":"+k2+":"+p2+":"+p)
        if (p1>p){
          val likelihoodRatio = logLikehoodRatio(N_S,k1,p1,N_words,k2,p2,p)
          //println(token._1+":"+token._3+":"+likelihoodRatio)
          if (likelihoodRatio>=30){
            Seq(token._3)
          }else{
            Nil
          }
        }else{
          Nil
        }
      })
    }

    def apply()={
      val mutableTokenList = collection.mutable.Map(tokenList.map(token=>token._2->token): _*)
      var filteredList = tokenList.filterNot(token=>token._1.equals("#number##.") || token._1.matches("""\p{L}\."""))
      var freqSentenceStarters = freqSentenceStarterHeuristic(filteredList).toSet
      freqSentenceStarters.foreach(println(_))
      collocationHeuristic(filteredList).filterNot{kv=> freqSentenceStarters(kv._3)}.foreach(colToken=>{
        mutableTokenList.update(colToken._2,colToken)
      })
      filteredList.foreach(token=>{
        if (token._4==A){
          println(token._1+" "+token._3+" "+token._4)
          val decision = decideOrthographic(token._3)
          println(decision)
          if (decision==SENTENCE_BOUNDARY || freqSentenceStarters(token._3))
            mutableTokenList.update(token._2,(token._1,token._2,token._3,AS))
        }
      })

      filteredList= tokenList.filter(token=>token._1.equals("##number##.") || token._1.matches("""\p{L}\."""))
      freqSentenceStarters = freqSentenceStarterHeuristic(filteredList).toSet
      collocationHeuristic(filteredList).filterNot{kv=> freqSentenceStarters(kv._3)}.foreach(colToken=>{
        mutableTokenList.update(colToken._2,colToken)
      })
      filteredList.foreach(token=>{
        if(token._4==S){
          println(token._1)
          val decision = decideOrthographic(token._3)
          println(decision)
          if (decision==NO_SENTENCE_BOUNDARY) mutableTokenList.update(token._2,(token._1,token._2,token._3,A))
          else if (decision==UNDECIDED && token._3.charAt(0).isUpper) mutableTokenList.update(token._2,(token._1,token._2,token._3,A))
        }
      })
      mutableTokenList.values.filter(_._4==A)
    }

  }

  def logLikehoodRatio(n1:Int,k1:Int,p1:Double,n2:Int,k2:Int,p2:Double,p:Double):Double={
    def logBino(n:Int,k:Int,p:Double) :Double = {
      if (p==0 || p==1) 0
      else k*math.log(p)+(n-k)*math.log(1-p)
    }
    2*(logBino(n1,k1,p1)+logBino(n2,k2,p2)-logBino(n1,k1,p)-logBino(n2,k2,p))
  }

  def createTypeBasedClassifier = new TypeBasedClassifier
  def createTokenBasedClassifier(tokenList:List[(String, Int, String, Int)]) = new TokenBasedClassifier(tokenList)

}

object SentenceBoundaryDetector{
  def apply(text:String)={
    val detector = new SentenceBoundaryDetector(text)
    val tokenList = detector.createTypeBasedClassifier.apply()
    detector.createTokenBasedClassifier(tokenList).apply()
  }
  def main(args:Array[String]) {
    val text = Source.fromFile("/Users/harshal/Work/file.txt").getLines().mkString(" ")
    SentenceBoundaryDetector(text).foreach(println(_))
  }
}