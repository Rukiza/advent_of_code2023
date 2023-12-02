import scala.io.Source
import scala.util.FromDigits
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break


@main def parse(input: String): Unit = {

  // val map: Map[String, Int] = Map("blue" -> 14, "red" -> 12, "green" -> 13)
  // val count: Int = 
  println("input file: "+input)

  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  val count: Int = lines
    .map(parseLine(_))
    .reduce(_ + _)

    println("Count: " + count)
}

def parseLine(line: String): Int = {

  var (gameNumber, index) = parseStart(line)

  val gameList: List[Map[String, Int]] = parseGameLine(line.substring(index))

  var map: scala.collection.mutable.Map[String, Int] = 
    scala.collection.mutable.Map("blue" -> 0, "red" -> 0, "green" -> 0)

  for gameMap <- gameList
    (key, value) <- gameMap do {
    // println(s"key: ${key}, value: ${value}")
    if (!map.contains(key)) {
      // println("Key not found")
    }
    if ((map.get(key) match {
      case None => 0
      case Some(v) => v
      }) < value) {
        map.addOne(key, value)
      }
  }

  map.values.reduce(_ * _)

}

def parseStart(line: String): (Int, Int) = {

  val end: Int = line.indexOf(':')
  val start: Int = 5

  var digit: String = ""
  for i <- start to end - 1 do {
    if (line(i).isDigit) {
      digit += line(i)
    }
  }

  (digit.toInt, end + 1)

}

def parseGameLine(line: String): List[Map[String, Int]] = {

  // var gameMap: Map[String, Int] = Map()
  // println("game line: "+line)
  
  val phases: Array[String] = line.split(";").map(_.trim)

  var array: ArrayBuffer[Map[String, Int]] = ArrayBuffer[Map[String, Int]]()

  for phase <- phases do {
    array.addOne(phase.split(",")
      .map(processTuple(_))
      .toMap)
  }

  array.toList
}


def processTuple(token: String): (String, Int) = {
  val splitToken: Array[String] = token.trim
    .split(" ")

  (splitToken(1), splitToken(0).toInt)
}

