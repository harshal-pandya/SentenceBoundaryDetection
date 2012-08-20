import io.Source
import org.apache.commons.math3.distribution.BinomialDistribution
import org.apache.commons.math3.util.{ArithmeticUtils, MathUtils}

/**
 * Created with IntelliJ IDEA.
 * User: harshal
 * Date: 8/15/12
 * Time: 7:58 PM
 * To change this template use File | Settings | File Templates.
 */

class SentenceBoundaryDetector(text: String) {
  val S = 1
  val A = 2
  val E = 3
  val regex = """\w+'[\.\-\w]+|\w+[\.\-\w]*"""
  val mutableMap = Set.empty[String]

  val tokenIt = regex.r.findAllIn(text)
  val mutableList = scala.collection.mutable.ListBuffer[(String,Int,String)]()
  var prevToken = ""
  while(tokenIt.hasNext) {
    val temp = tokenIt.next()
    mutableList++=Seq((temp,tokenIt.start,prevToken))
    prevToken = temp
  }
  val tokens = mutableList.toList

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

    def applyTypeBasedClassifier:String ={
      val textBuf = new StringBuilder(text.length)
      var lastPos = 0
      //val tokenMap = tokens.groupBy(x => x._1.toLowerCase)
      val replaceList = likelihoodRatios.flatMap(kv=> {
       if(kv._2>=0.3){
         TOKEN_MAP.get(kv._1+".").get.map(token=>{
           (token._1,token._2,token._3,A)
//           println(token._1+" : "+token._2)
//           textBuf.append(text.substring(lastPos,token._2)+token._1+"<A>")
//           lastPos=token._2+(token._1).length+3
         })
       }else{
         TOKEN_MAP.get(kv._1+".").get.map(token=>{
           (token._1,token._2,token._3,S)
//           println(token._1+" : "+token._2)
//           textBuf.append(text.substring(lastPos,token._2)+token._1+"<S>")
//           lastPos=token._2+(token._1).length+3
         })
       }
      }).toList
      replaceList.sortBy(_._2).foreach(elem=>{
        println(elem._1+" : "+elem._2+" : "+lastPos)
        textBuf.append(text.substring(lastPos,elem._2)+elem._1)
        lastPos=elem._2+(elem._1).length-3

      })
      textBuf.append(text.substring(lastPos))
      return textBuf.toString()
    }

  }




  def getWordList = new TypeBasedClassifier().likelihoodRatios.toList sortBy{_._2}

  def getOutput = new TypeBasedClassifier().applyTypeBasedClassifier

  class TokenBasedClassifier(text:String,tokenMap:List[(String, Int, String, Int)]) {
    //val sList = """(?<=<S>)\b.+?\b""".r findAllIn(text) toSet
    //val text = text.replaceAll("""<[AES]>.*(?!\w)""","")
    val aList = """(?<=<[A]>)\s*.+?(\w+'[\.\-\w]+|\w+[\.\-\w]*)""".r findAllIn(text) toList //"""(?<=<A>)\b.+?\b""".r findAllIn(text) toList
    val eList = """(?<=<[A]>)\s*.+?(\w+'[\.\-\w]+|\w+[\.\-\w]*)""".r findAllIn(text) toList
    val aeList = aList++eList
    val UNDECIDED = 0
    val SENTENCE_BOUNDARY = 1
    val NO_SENTENCE_BOUNDARY = 2
    def decideOrthographic(token:String):Int ={
      if(token.charAt(0).isUpper){
        if((token.toLowerCase.r findFirstIn(this.text)).nonEmpty){
          if(("""(?<!<[SAE]>)\b"""+token+"""\b""".r.findFirstIn(this.text)).isEmpty){
            SENTENCE_BOUNDARY
          }else{
            UNDECIDED
          }
        }else{
          UNDECIDED
        }
      }else{
        if ((token.capitalize.r findFirstIn(this.text)).nonEmpty || ("""(?<=<S>)"""+token+"""\b""".r.findFirstIn(this.text)).isEmpty ){
          return NO_SENTENCE_BOUNDARY
        }else{
          return UNDECIDED
        }
      }
    }
    def collocationHeuristic{
      val collocations = """\b.+?\b(?<=<[SAE]>)\b.+?\b""".r.findAllIn(text).toList.flatMap(w=> {
        val temp = w.split("""\.+<[AES]>""").map(_.trim.toLowerCase)
        temp(0).trim -> temp(1)
        //val i1 = wordCount.getOrElse(temp(0),0)
        val i2 = wordCount.getOrElse(temp(1),0)
        val k1 = """(?i)\b"""+temp(0)+"""\b(([\p{P}\s]*)|(\.<[AES]>))\b"""+temp(1)+"""\b""".r.findAllIn(text) length
        val k2 = i2 - k1
        val n1 = wordCount.getOrElse(temp(0),0)
        val n2 = N_words - n1
        val p1 = k1.asInstanceOf[Double]/n1
        val p2 = k2.asInstanceOf[Double]/n2
        val p = i2.asInstanceOf[Double]/N
        if (p1>p){
          val likelihoodRatio = SentenceBoundaryDetectorUtils.logLikehoodRatio(n1,k1,p1,n2,k2,p2,p)
          if (likelihoodRatio>=7.88){
            Seq(w->likelihoodRatio)
          }else{
            Nil
          }
        }else{
          Nil
        }
      }).toMap
    }
    def freqSentenceStarterHeuristic(text:String){
      val N_S = """<S>""".r.findAllIn(text) length

      """(?<=<S>).+?\b""".r.findAllIn(text).toList.flatMap(w=>{
        val i2 = wordCount.getOrElse(w.toLowerCase,0)
        val k1 = """(?i)<S>"""+w+"""\b""".r.findAllIn(text) length
        val k2 = i2 - k1
        val p1 = k1.asInstanceOf[Double]/N_S
        val p2 = k2.asInstanceOf[Double]/N_words
        val p = i2.asInstanceOf[Double]/(N_S+N_words)
        if (p1>p){
          val likelihoodRatio = SentenceBoundaryDetectorUtils.logLikehoodRatio(N_S,k1,p1,N_words,k2,p2,p)
          if (likelihoodRatio>=30){
            Seq(w)
          }else{
            Nil
          }
        }else{
          Nil
        }
      })
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
    val text = Source.fromFile("/Users/harshal/Work/file.txt").getLines().mkString
    println(new SentenceBoundaryDetector(text).getOutput)
    //new SentenceBoundaryDetector(text).printWordCount
  }
}

