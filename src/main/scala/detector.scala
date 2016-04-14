// TODO: way to store dictionnary in a text file?

object Detector {

  def main(args: Array[String]) = {
    println("Compile?")
  }

  // methods building features
  // 1. simple letter freq: Map letter->frequency
  def countFreq(text: List[String]):Map[Char,Double] = {
    val count = "abcdefghijklmnopqrtstuvwxyz".toList
    return count.map((c:Char)=>(c->text.mkString.count(_==c).toDouble/
                                   text.mkString.length.toDouble)).toMap
  }
  // 2. letter transition frequency: Map (letter1,letter2)->frequency
  def countTrans(text: List[String]):Map[(Char,Char),Double] = {

  }
  def find_language(text:String,countRef: Map[String, Map[Char, Double]]):Iterable[String] = {
    val c = countFreq(text.toList.map(_.toString))
    return countRef.filter((count)=>compareFreq(count._2,c)==countRef.values.map(compareFreq(_,c)).min).keys
  }
}
