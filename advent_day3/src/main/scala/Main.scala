import scala.util.boundary, boundary.break

case class Location(row: Int, column: Int)

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
    if (line(i) == '*') {
      val locations: Option[(Location, Location)] = 
        findDigits(lines, index, i)
      // sum = sum + (if (included) {number} else {0})
      // i = newIndex
      locations match {
        case None => ()
        case Some((first, second)) => {
          val firstDigit = parseDigit(lines(first.row), first.column)
          val secondDigit = parseDigit(lines(second.row), second.column)
          sum += firstDigit * secondDigit
        }
      }
    }
    i += 1
  }

  sum
}

def findDigits(lines: Seq[String], row: Int, column: Int): Option[(Location, Location)] = {

  var first: Option[Location] = None

  // val northWest: Option[Location] = checkDigit(lines, row-1, column-1)
  // val north: Option[Location] = checkDigit(lines, row-1, column)
  // val northEast: Option[Location] = checkDigit(lines, row-1, column+1)

  // val west: Option[Location] = checkDigit(lines, row, column-1)
  // val east: Option[Location] = checkDigit(lines, row, column+1)

  // val southWest: Option[Location] = checkDigit(lines, row+1, column-1)
  // val south: Option[Location] = checkDigit(lines, row+1, column)
  // val southEast: Option[Location] = checkDigit(lines, row+1, column+1)

  val toCheck: List[(Int, Int)] = 
    List(
      (row-1, column-1),
      (row-1, column),
      (row-1, column+1),
      (row, column-1),
      (row, column+1),
      (row+1, column-1),
      (row+1, column),
      (row+1, column+1)
    )

  val found = toCheck
    .map((r, c) => checkDigit(lines, r, c))
    .toSet
    .flatMap(x => x)
    
  if (found.size < 2 || found.size > 2) {
    None
  } else {
    Some((found.head, found.tail.head))
  }
  
  // (northWest, north, northEast) match {
    // case (None, None, None) => _
    // case (Some())
  // }
}

def parseDigit(line: String, index: Int): Int = {
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

  digit.toInt
}
def checkDigit(lines: Seq[String], row: Int, column: Int): Option[Location] = {

  if (row < 0 || row > lines.length -1) {
    None
  }
  else if (column < 0 || column > lines(row).length -1) {
    None
  }
  else {
    if (lines(row)(column).isDigit) {
      checkDigit(lines, row, column -1) match {
        case None => Some(Location(row, column))
        case Some(l) => Some(l)
      }
    } 
    else {
      None
    }
  }
}

// def confirmIncluded(lines: Seq[String], start: Int, end: Int, row: Int): Boolean = {
//   boundary:
//     for i <- row -1 to row + 1
//         j <- start to end do {
//         if (i < 0 || i > lines.length -1) {}
//         else if (j < 0 || j > lines(i).length -1) {}
//         else if (!lines(i)(j).isLetterOrDigit && lines(i)(j) != '.') {
//           // println("confirm included" + lines(i)(j))
//           break(true)
//         }
//       }
//     false
// }

