import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

def parseLines(lines: Seq[String]): Int = {
  val times: List[Int] = parseString(lines, 0)
  val distances: List[Int] = parseString(lines, 1)

  println(times)
  println(distances)
  var total = 1
  for i <- 0 to times.length - 1 do {
    var sum = 0
    for t <- 0 to times(i) - 1 do {
      if (t * (times(i) - t) > distances(i)) {
        sum += 1
      }
    }
    println(sum)
    total = total * sum
  }
  total
}

def parseString(lines: Seq[String], index: Int): List[Int] = {
  val parsed: List[Int] = lines(index)
    .split(':')(1)
    .split(' ')
    .filterNot(_ == "")
    .map(_.toInt)
    .toList

  parsed
}


// Understanding.. 
// min is 
// T D 
// floor(D / T) + 1 recuse until D / T grather than Prev floor(D/T) + 1
//
//

