import io.Source
import language.postfixOps

// TODO: fix main to adapt to new function definitions

object Detector {

  // reading dictionnary
  def getDic = {
    val readf = Source.fromFile("src/main/dict.txt").getLines.toList map {
      _.split(" ").toList}
    val lang = readf.map(_.apply(0)).distinct
    lang.map((l:String)=>(l,readf filter{_(0)==l} map(
      (lis)=>(lis(1)(0),lis(2).toDouble))toMap)).toMap
  }

  val dic = getDic
  def main(args: Array[String]) = {
    println("Starting program")
    println("Possible languages")
    dic.keys foreach println
    val text = io.StdIn.readLine
    // computing note for each language
    println("--- Languages scoring: L1 ---")
    val notes1 = comp_language(text,dic,compareL1)
    notes1.toSeq.sortWith(_._1 < _._1).foreach(tup=>println(""+tup._1+": "+tup._2))
    val l1 = find_language(text,dic,compareL1)
    println("----Decision----")
    print("Language: ")
    println(l1)
    print("Score: ")
    println(compareL1(dic.apply(l1),countFreq(text)))

    println("--- Languages scoring: L2 ---")
    val notes2 = comp_language(text,dic,compareL2)
    notes2.toSeq.sortWith(_._1 < _._1).foreach(tup=>println(""+tup._1+": "+tup._2))
    val l2 = find_language(text,dic,compareL2)
    println("----Decision----")
    print("Language: ")
    println(l2)
    print("Score: ")
    println(compareL2(dic.apply(l2),countFreq(text)))

  }

  // methods building features
  // 1. simple letter freq: Map letter->frequency
  def countFreq(text: String):Map[Char,Double] = {
    val count = "abcdefghijklmnopqrtstuvwxyz".toList
    return count.map((c:Char)=>(c->text.count(_==c).toDouble/
                                   text.length.toDouble)).toMap
  }

  def countTrans(text: String):Map[(Char,Char),Double] = {
    val letters = "abcdefghijklmnopqrtstuvwxyz".toList
    val transitions = letters map {letter => letters map {(letter,_)}} flatten
    val textTrans = Range(0,text.length-1) map {ind:Int => (text.apply(ind),text.apply(ind+1))}
    return transitions.map((t:(Char,Char))=>(t->textTrans.count(_==t).toDouble/
                                   textTrans.length.toDouble)).toMap
  }

  // Computes the score of each language for the text
  def comp_language(text:String,countRef: Map[String, Map[Equals, Double]],
    compFunc: Function2[Map[Equals,Double],Map[Equals,Double],Double],
    compText:Function1[String,Map[Equals,Double]]):
      Map[String,Double] = {
    val cText = compText(text)
    return countRef map {tup => (tup._1,compFunc(tup._2,cText))}
  }

  // finds the best fitting language from the frequency count and
  // def find_language(text:String,countRef: Map[String, Map[Char, Double]],
  //   compFunc: Function2[Map[Char,Double],Map[Char,Double],Double]):
  //   String = {
  //   val c = countFreq(text)
  //   return countRef.filter((count)=>compFunc(count._2,c)==
  //     countRef.values.map(compFunc(_,c)).min).keys.toList.apply(0)
  // }

  // finds the best fitting language from the score of each one
  def find_language(scores: Map[String,Double]):
    String = {
    return scores.filter(_==scores.values.min).apply(0)
  }

  def compareL1(countLang:Map[Equals,Double],countText:Map[Equals,Double]):
    Double = {
    val mapfreq = countLang.keys.map((el:Equals)=>Math.abs(countLang.apply(el)-
    countText.apply(el)))
    return 1.0*mapfreq.sum/mapfreq.size
  }

  def compareL2(countLang:Map[Equals,Double],countText:Map[Equals,Double]):
    Double = {
    val mapfreq = countLang.keys.map((el:Equals)=>Math.pow(countLang.apply(el)-
    countText.apply(el),2))
    return 1.0*mapfreq.sum/mapfreq.size
  }
  // def compareFreq(countLang:Map[Char,Double],countText:Map[Char,Double]):
  //   Double = {
  //   val mapfreq = countLang.keys.map((c:Char)=>Math.abs(countLang.apply(c)-
  //   countText.apply(c)))
  //   return mapfreq.sum/mapfreq.size
  // }

}

// 2. letter transition frequency: Map (letter1,letter2)->frequency
// include letter transition from and to whitespace
// def countTrans(text: List[String]):Map[(Char,Char),Double] = {
//
// }

// object DicBuilder {
//
// }
