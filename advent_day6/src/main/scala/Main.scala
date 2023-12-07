import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

