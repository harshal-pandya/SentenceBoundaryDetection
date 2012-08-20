import io.Source
import org.apache.commons.math3.distribution.BinomialDistribution
import org.apache.commons.math3.util.{ArithmeticUtils, MathUtils}
import util.matching.Regex

class SentenceBoundaryDetector(text: String) {
  val S = 1
  val A = 2
  val E = 3


  def getTokens(text:String)={
    val regex = """\w+'[\.\-\w]+|\w+[\.\-\w]*"""
    val tokenIt = regex.r.findAllIn(text)
    val tokenList = regex.r.findAllIn(text).toList
    val mutableList = scala.collection.mutable.ListBuffer[(String,Int,String)]()
    val mutableSet = scala.collection.mutable.Set[String]()
    var index = 1
    while(tokenIt.hasNext) {
      val temp = tokenIt.next()

      if (index==tokenList.length)
        mutableList++=Seq((temp,tokenIt.start,""))
      else
        mutableList++=Seq((temp,tokenIt.start,tokenList(index)))

      mutableSet.add(temp)
      index+=1
    }
    (mutableList.toList,mutableSet.toSet)
  }

  val (tokens,tokenStrings) = getTokens(text)
  //val tokenStrings = tokens.map(_._1).toSet

  val TOKEN_MAP = tokens.groupBy(x => x._1.toLowerCase)

  val wordCount = tokens.groupBy(x => x._1.toLowerCase).mapValues(_.length)

  val periodCount = wordCount.filter {
    kv => kv._1.endsWith(".")
  }.foldLeft(0)((acc, kv) => acc + kv._2)

  val N_words = tokens.length
  val N = N_words+ periodCount

  class TypeBasedClassifier {

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
        Seq(w -> SentenceBoundaryDetectorUtils.logLikehoodRatio(n1,k1,p1,n2,k2,p2,p)*fLength*fPeriods)
      }
      else Nil
    })

    def applyTypeBasedClassifier ={
      val textBuf = new StringBuilder(text.length)
      var lastPos = 0
      //val tokenMap = tokens.groupBy(x => x._1.toLowerCase)
      likelihoodRatios.flatMap(kv=> {
       if(kv._2>=0.3){
         TOKEN_MAP.get(kv._1+".").get.map(token=>{
           (if(token._1.endsWith("."))token._1.substring(0,token._1.length-1) else token._1,
            token._2,
            if(token._3.endsWith("."))token._3.substring(0,token._3.length-1) else token._3,
            A)
         })
       }else{
         TOKEN_MAP.get(kv._1+".").get.map(token=>{
           (if(token._1.endsWith("."))token._1.substring(0,token._1.length-1) else token._1,
            token._2,
            if(token._3.endsWith("."))token._3.substring(0,token._3.length-1) else token._3,
            S)
         })
       }
      }).toList
//      replaceList.sortBy(_._2).foreach(elem=>{
//        println(elem._1+" : "+elem._2+" : "+lastPos)
//        textBuf.append(text.substring(lastPos,elem._2)+elem._1)
//        lastPos=elem._2+(elem._1).length-3
//
//      })
//      textBuf.append(text.substring(lastPos))
//      return textBuf.toString()
    }

  }




  def getWordList = {
    new TokenBasedClassifier(new TypeBasedClassifier().applyTypeBasedClassifier).applyTokenBasedClassifier
  }

  def getOutput = new TypeBasedClassifier().applyTypeBasedClassifier

  class TokenBasedClassifier(var tokenMap:List[(String, Int, String, Int)]) {
    //val sList = """(?<=<S>)\b.+?\b""".r findAllIn(text) toSet
    //val text = text.replaceAll("""<[AES]>.*(?!\w)""","")
    //val aList = """(?<=<[A]>)\s*.+?(\w+'[\.\-\w]+|\w+[\.\-\w]*)""".r findAllIn(text) toList //"""(?<=<A>)\b.+?\b""".r findAllIn(text) toList
    //val eList = """(?<=<[A]>)\s*.+?(\w+'[\.\-\w]+|\w+[\.\-\w]*)""".r findAllIn(text) toList
    //val aeList = aList++eList
    val sSet = tokenMap.filter(_._4==S).map(_._3).toSet
    val aSet = tokenMap.filter(_._4==A).map(_._3).toSet
    val internalWords = tokenStrings.diff(aSet.intersect(sSet))
    val UNDECIDED = 0
    val SENTENCE_BOUNDARY = 1
    val NO_SENTENCE_BOUNDARY = 2
    def decideOrthographic(token:String):Int ={
      if(token.charAt(0).isUpper){
        //if((token.toLowerCase.r findFirstIn(this.text)).nonEmpty){
        if(tokenStrings(token.toLowerCase)){
          //if(("""(?<!<[SAE]>)\b"""+token+"""\b""".r.findFirstIn(this.text)).isEmpty){
          if(!internalWords(token)){
            SENTENCE_BOUNDARY
          }else{
            UNDECIDED
          }
        }else{
          UNDECIDED
        }
      }else{
        //if ((token.capitalize.r findFirstIn(this.text)).nonEmpty || ("""(?<=<S>)"""+token+"""\b""".r.findFirstIn(this.text)).isEmpty ){
        if(tokenStrings(token.charAt(0).toUpper+token.substring(1)) || !sSet(token)){
          return NO_SENTENCE_BOUNDARY
        }else{
          return UNDECIDED
        }
      }
    }
    def collocationHeuristic={
      tokenMap.flatMap(token=> {
        val token1 = if(token._1.endsWith("."))token._1.substring(0,token._1.length-1) else token._1
        val token2 = if(token._3.endsWith("."))token._3.substring(0,token._3.length-1) else token._3
        //val temp = w.split("""\.+<[AES]>""").map(_.trim.toLowerCase)
        //val i1 = wordCount.getOrElse(temp(0),0)

        val i2 = wordCount.getOrElse(token2.toLowerCase,0)+wordCount.getOrElse(token2.toLowerCase+".",0)
        //val k1 = """(?i)\b"""+temp(0)+"""\b(([\p{P}\s]*)|(\.<[AES]>))\b"""+temp(1)+"""\b""".r.findAllIn(text) length
        val k1 = tokenMap.filter(t=> t._1.equals(token._1) && t._3.equals(token._3)) length
        val k2 = i2 - k1
        val n1 = wordCount.getOrElse(token1.toLowerCase,0)+wordCount.getOrElse(token1.toLowerCase+".",0)
        val n2 = N_words - n1
        val p1 = k1.asInstanceOf[Double]/n1
        val p2 = k2.asInstanceOf[Double]/n2
        val p = i2.asInstanceOf[Double]/N
        if (p1>p){
          val likelihoodRatio = SentenceBoundaryDetectorUtils.logLikehoodRatio(n1,k1,p1,n2,k2,p2,p)
          if (likelihoodRatio>=7.88){
            Seq((token._1,token._3)->likelihoodRatio)
          }else{
            Nil
          }
        }else{
          Nil
        }
      }).toMap
    }
    def freqSentenceStarterHeuristic={
      val N_S = tokenMap.filter(_._4==S) length

      tokenMap.flatMap(token=>{
        val w = if(token._1.endsWith("."))token._1.substring(0,token._1.length-1) else token._1
        val i2 = wordCount.getOrElse(w.toLowerCase,0)+wordCount.getOrElse(w.toLowerCase+".",0)
        val k1 = tokenMap.filter(t=> t._3.equals(token._3) && t._4==S) length
        val k2 = i2 - k1
        val p1 = k1.asInstanceOf[Double]/N_S
        val p2 = k2.asInstanceOf[Double]/N_words
        val p = i2.asInstanceOf[Double]/(N_S+N_words)
        if (p1>p){
          val likelihoodRatio = SentenceBoundaryDetectorUtils.logLikehoodRatio(N_S,k1,p1,N_words,k2,p2,p)
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

    def applyTokenBasedClassifier={
      tokenMap = tokenMap.map(token=>{
        if (token._3==A){
          val decision = decideOrthographic(token._3)
          if (decision==SENTENCE_BOUNDARY) {
            println(token._1+" : "+token._3)
            (token._1,token._2,token._3,S)}
          else if (decision==NO_SENTENCE_BOUNDARY) (token._1,token._2,token._3,A)
          else (token._1,token._2,token._3,token._4)
        }else{
          (token._1,token._2,token._3,token._4)
        }
      })
      val collocations = collocationHeuristic
      val freqSentenceStarters = freqSentenceStarterHeuristic.toSet
      collocations.filterNot{kv=> freqSentenceStarters(kv._1._2) }
    }

  }

}
object SentenceBoundaryDetectorUtils{
  def logLikehoodRatio(n1:Int,k1:Int,p1:Double,n2:Int,k2:Int,p2:Double,p:Double):Double={

    def logBino(n:Int,k:Int,p:Double) :Double = k*math.log(p)+(n-k)*math.log(1-p)

    2*(logBino(n1,k1,p1)+logBino(n2,k2,p2)-logBino(n1,k1,p)-logBino(n2,k2,p))
  }
}
object SentenceBoundaryDetector{
  def main(args:Array[String]) = {
    val text = Source.fromFile("/Users/harshal/Work/file1.txt").getLines().mkString
    //println(new SentenceBoundaryDetector(text).getOutput)
    //println(new SentenceBoundaryDetector(text).getTokens(text)._1
    println(new SentenceBoundaryDetector(text).getWordList)

  }
}

