import scala.util.boundary, boundary.break

@main def parse(input: String): Unit = {

  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  
  println(parseLines(lines))

}

def parseLines(lines: Seq[String]): Int = {

  var sum: Int = 0
  for i <- 0 to lines.length -1  do {
    sum += parseLine(lines, lines(i), i)
  }

  sum
}

def parseLine(lines: Seq[String], line: String, index: Int): Int = {
  var i = 0
  var sum = 0
  while i < line.length() do {
    if (line(i).isDigit) {
      val (newIndex, number) = parseDigit(line, i)
      val included: Boolean = confirmIncluded(lines, i - 1, newIndex, index)
      sum = sum + (if (included) {number} else {0})
      i = newIndex
    }
    i += 1
  }

  sum
}

def parseDigit(line: String, index: Int): (Int, Int) = {
  var digit: String = ""
  var i = index
  boundary:
    while i < line.length do {
      if (line(i).isDigit) {
        digit += line(i)
      }
      else {
        break()
      }
      i += 1
    }

  (i, digit.toInt)
}

def confirmIncluded(lines: Seq[String], start: Int, end: Int, row: Int): Boolean = {
  boundary:
    for i <- row -1 to row + 1
        j <- start to end do {
        if (i < 0 || i > lines.length -1) {}
        else if (j < 0 || j > lines(i).length -1) {}
        else if (!lines(i)(j).isLetterOrDigit && lines(i)(j) != '.') {
          // println("confirm included" + lines(i)(j))
          break(true)
        }
      }
    false
}

