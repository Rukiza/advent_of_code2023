import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

def parseLines(lines: Seq[String]): Int = {
  var map: Map[Int, Int] = Map()
  var keys: ArrayBuffer[Int] = ArrayBuffer()
  for (line <- lines) do  {
    val (card, matches) = parseCard(line)
    keys.addOne(card) 
    val extraGames = map.get(card) match {
      case None => 0
      case Some(value) => value
    }
    println(s"card: ${card} extraGames: ${extraGames}")
    for (i <- card + 1 to card + matches) do {
      var temp = map.get(i) match {
        case None => (i, 1 + extraGames)
        case Some(value) => {
          (i, value + 1 + extraGames)
        }
      }
      map.addOne(temp)
    }
    map.addOne((card, extraGames + 1))
  }
  keys.map(map.get(_))
   .flatMap(x => x)
   .reduce(_ + _)
}

def parseCard(line: String): (Int, Int) = {
  val splitString: Array[String] = line.split(":")
  val (cardStr, game) = (splitString(0), splitString(1))
  val card: Int = cardStr.split(" ")
    .filterNot(_ == "")(1)
    .trim
    .toInt
  (card, parseGameLine(game))
}

def parseGameLine(game: String): Int = {
  val split: Array[String] = game.split('|')
  val (winnersStr, oursStr) = (split(0).trim, split(1).trim)
  val winner = winnersStr.split(' ')
    .filterNot(_ == "")
    .map(_.trim)
    .map(_.toInt)
  val ours = oursStr.split(' ')
    .filterNot(_ == "")
    .map(_.trim)
    .map(_.toInt)
  calculateWin(winner, ours) match {
    case x if x.isEmpty => 0
    case x => x.reduce(_ + _)
  }
}

def calculateWin(winner: Array[Int], ours: Array[Int]): ArrayBuffer[Int] = {
  val array: ArrayBuffer[Int] = ArrayBuffer[Int]()
  var first = false
  for (value <- ours) do {
    if (winner.contains(value)) {      
      array.addOne(1)
    }
  }
  array
}

