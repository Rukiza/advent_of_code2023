import scala.io.Source

@main def parse(input: String): Unit = {
  val count: Int = Source
    .fromFile(input)
    .getLines
    .map(firstAndLastInt)
    .reduceLeft(_ + _)
  println(count)
}

def firstAndLastInt(line: String): Int = {
  val firstIndex = line.indexWhere(_.isDigit)
  val firstChar: Char = line(firstIndex)
  val lastIndex = line.lastIndexWhere(_.isDigit)
  val lastChar: Char = line(lastIndex)

  (s"${firstChar}${lastChar}").toInt
}

