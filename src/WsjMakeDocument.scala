import io.Source
import java.io.{File, PrintWriter}

object WsjMakeDocument {
  def main(args: Array[String]) {
    val buffer = new StringBuilder
    val abvList = new scala.collection.mutable.ListBuffer[String]()
    for (line <- Source.fromFile("/Users/harshal/Work/data/wsj/wsj_dev_19-21.txt").getLines()){
      if (!line.equals("")){
        val word = line.split("WORD=")(1)
        if (!word.equals("LRB-") && !word.equals("RRB-") && !word.equals("LCB-") && !word.equals("RCB-") && !word.matches("""\.\.+""")){
          if (!word.equals(".") && word.endsWith(".")) abvList++=Seq(word)
          if (word.equals(".") || word.startsWith("'")) buffer.append(word)
          else if ((!word.equals(".") && word.endsWith("."))) buffer.append(" "+word+"##abv##")
          else buffer.append(" "+word)
        }
      }
    }
    writeToFile("/Users/harshal/Work/data/wsj/wsj_text.txt",buffer.mkString)
    println("Started....")
    val startTime = System.currentTimeMillis()
    val list = SentenceBoundaryDetector(buffer.mkString).toList//.sortBy(_._2)

    println("Time elapsed: "+((System.currentTimeMillis()-startTime)/60000.0))
//    val genSet = list.map(_._1).toSet
//    val abvSet = abvList.toSet
//    println("True List size: "+abvList.size)
//    println("Generated List size: "+list.size)
//    println("True Set Size: "+abvSet.size)
//    println("Generate Set Size: "+genSet.size)
//    println("No of common abvs: "+abvSet.intersect(genSet).size)
//    println("List of false negatives:")
//    abvSet.diff(genSet).foreach(println(_))
//    println("List of false positives:")
//    genSet.diff(abvSet).foreach(println(_))

    val truePos = list.filter(token=> token._4==2 && token._5).length
    val falsePos = list.filter(token=> token._4==2 && !token._5).length
    //val trueNeg = list.filter(token=> token._4!=2 && !token._5).length
    val falseNeg = list.filter(token=> token._4!=2 && token._5).length

    val errorRate = (falsePos+falseNeg).asInstanceOf[Double]/list.length
    val precision = truePos.asInstanceOf[Double]/(truePos+falsePos)
    val recall = truePos.asInstanceOf[Double]/(truePos+falseNeg)
    val f = (2*precision*recall)/(precision+recall)

    println("**************************")
    println("* Error Rate :\t"+errorRate*100)
    println("* Precision  :\t"+precision)
    println("* Recall     :\t"+recall)
    println("* F1         :\t"+f)


//    for (i<-0 to abvList.length-1){
//      if (i>=list.size){
//        println(abvList(i))
//      }else{
//        println(abvList(i)+"\t"+list(i))
//      }
//    }

  }

  def writeToFile(file:String,str:String){
    val writer = new PrintWriter(new File(file))

    writer.write(str)
    writer.close()
  }
}
