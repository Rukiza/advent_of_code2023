import scala.collection.mutable.ArrayBuffer

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)

  println(lines
    .map(parseGameLine)
    .reduce(_ + _))
}

def parseGameLine(line: String): Int = {

  val game: String = line.split(":")(1)
  println(game)
  val split: Array[String] = game.split('|')
  split.map(println(_))
  val (winnersStr, oursStr) = (split(0).trim, split(1).trim)

  println(winnersStr)
  println(oursStr)

  val winner = winnersStr.split(' ')
    .filterNot(_ == "")
    .map(_.trim)
    .map(_.toInt)
  val ours = oursStr.split(' ')
    .filterNot(_ == "")
    .map(_.trim)
    .map(_.toInt)

  calculatePoints(winner, ours) match {
    case x if x.isEmpty => 0
    case x => x.reduce(_ * _)
  }

}

def calculatePoints(winner: Array[Int], ours: Array[Int]): ArrayBuffer[Int] = {

  val array: ArrayBuffer[Int] = ArrayBuffer[Int]()
  var first = false

  for (value <- ours) do {
    println("value: "+value)
    if (!first && winner.contains(value)) {
      array.addOne(1)
      first = true
    } 
    else if (winner.contains(value)) {      
      array.addOne(2)
    }
  }

  array

}

