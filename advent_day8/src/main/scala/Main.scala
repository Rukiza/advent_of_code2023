import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.util.boundary, boundary.break

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

case class Node(left: String, right: String)

// var network: Map[String, Node] = Map()

def parseLines(lines: Seq[String]): Int = {
  val path: List[Char] = lines(0)
    .split("")
    .flatMap(x => x.toCharArray).toList

  val nodeLines = lines.drop(2).toSeq

  val network = parseNodes(nodeLines)

  val steps: ArrayBuffer[String] = ArrayBuffer("AAA")

  var i: Int = 0
  boundary:
    while (i <= path.length) do {
      if (i >= path.length) {
        i = 0 // We go round again 
      }
      // if (steps.length < 4) {
      //   println(steps)
      //   println(i)
      //   println(path)
      // }
      val node = network.get(steps.last) match {
        case None => Node("", "") // Should never get here.
        case Some(v) => v
      }
      path(i) match {
        case 'L' => steps.addOne(node.left)
        case 'R' => steps.addOne(node.right)
      }
      if (steps.last == "ZZZ") {
        break(())
      }
      i += 1
    }

  steps.length - 1
}

def parseNodes(nodeLines: Seq[String]): Map[String, Node] = {
  var network: Map[String, Node] = Map()

  for n <- nodeLines do {
    val temp = n.split(" = ")
    val nodeLF = temp(1).split(", ")
    network.addOne(
      temp(0),
      Node(
        nodeLF(0).drop(1).toString,
        nodeLF(1).dropRight(1).toString
      )
    )

  }

  network
}

