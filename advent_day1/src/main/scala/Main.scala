import scala.io.Source
import scala.util.FromDigits
import scala.util.boundary, boundary.break

@main def parse(input: String): Unit = {
  val count: Int = Source
    .fromFile(input)
    .getLines
    .map(firstAndLastInt)
    .reduceLeft(_ + _)
  println(count)
}

enum Digits:
  case one, two, three, four, five, six, seven, eight, nine

def firstAndLastInt(line: String): Int = {
  (findFirst(line) + findLast(line)).toInt
}

def findFirst(line: String): String = {

  boundary:
    for i <- 0 to line.length() do {
      line(i) match {
        case x if x.isDigit => break(s"${line(i)}")
        case x if matchAll(Digits.one, line, i, x, true) => break("1")
        case x if matchAll(Digits.two, line, i, x, true) => break("2")
        case x if matchAll(Digits.three, line, i, x, true) => break("3")
        case x if matchAll(Digits.four, line, i, x, true) => break("4")
        case x if matchAll(Digits.five, line, i, x, true) => break("5")
        case x if matchAll(Digits.six, line, i, x, true) => break("6")
        case x if matchAll(Digits.seven, line, i, x, true) => break("7")
        case x if matchAll(Digits.eight, line, i, x, true) => break("8")
        case x if matchAll(Digits.nine, line, i, x, true) => break("9")
        case _ => true
      }  
    } 
    ""
}

def findLast(line: String): String = {

  boundary:
    for i <- 0 to line.length() do {
      val n = line.length() - i -1 
      line(n) match {
        case x if x.isDigit => break(s"${line(n)}")
        case x if matchAll(Digits.one, line, n, x, false) => break("1")
        case x if matchAll(Digits.two, line, n, x, false) => break("2")
        case x if matchAll(Digits.three, line, n, x, false) => break("3")
        case x if matchAll(Digits.four, line, n, x, false) => break("4")
        case x if matchAll(Digits.five, line, n, x, false) => break("5")
        case x if matchAll(Digits.six, line, n, x, false) => break("6")
        case x if matchAll(Digits.seven, line, n, x, false) => break("7")
        case x if matchAll(Digits.eight, line, n, x, false) => break("8")
        case x if matchAll(Digits.nine, line, n, x, false) => break("9")
        case _ => true
      }  
    }
    ""
}

def matchAll(digit: Digits, line: String, i: Int, c: Char, left: Boolean): Boolean = {
  val digitFirst = digit.toString()(0)
  c == digitFirst && 
  matchRest(digit.toString(), line, i + 1, left)
}

def matchRest(digit: String, line: String, i: Int, left: Boolean): Boolean = {
  val length = line.length()
   
  if (length < i + digit.length() - 1) {
    return false
  }



  var m = 1
  boundary:
    for n <- i to (i + digit.length() - 2) do {
      if (line(n) != digit(m)) {
        break(false)
      }
      m = m + 1
    }

    true
}

