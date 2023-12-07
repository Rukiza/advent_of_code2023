import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

def parseLines(lines: Seq[String]): Long = {
  val times: Long = parseString(lines, 0)
  val distances: Long = parseString(lines, 1)

  println(times)
  println(distances)
  var total: Long = 1
  var sum: Long = 0
  for t <- 0L to times - 1 do {
    if (t * (times - t) > distances) {
      sum += 1
    }
  }
  total = total * sum
  total
}

def parseString(lines: Seq[String], index: Int): Long = {
  val parsed: Long = lines(index)
    .split(':')(1)
    .split(' ')
    .filterNot(_ == "")
    .foldLeft("")((a,b) => s"${a}${b}").toLong
  parsed
}


// Understanding.. 
// min is 
// T D 
// floor(D / T) + 1 recuse until D / T grather than Prev floor(D/T) + 1
//
//

