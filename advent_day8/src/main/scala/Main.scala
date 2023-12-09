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


def parseLines(lines: Seq[String]): BigInt = {
  val path: List[Char] = lines(0)
    .split("")
    .flatMap(x => x.toCharArray).toList

  val nodeLines = lines.drop(2).toSeq

  val network = parseNodes(nodeLines)

  network
    .keys
    .filter(s => s.matches("..A"))
    .to(ArrayBuffer)
    .map(x => run(x, network, path))
    .foldLeft(1:BigInt)({
      (a,b) => b*a / Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs // Not my code.. works good though. 
    })
}

def run(start: String, network: Map[String, Node], path: List[Char]): BigInt = {
  var counter = BigInt(0)
  var i: Int = 0
  var current = start
  boundary:
    while (i <= path.length) do {
      if (i >= path.length) {
        i = 0 // We go round again 
      }
      val node = network.get(current) match {
        case None => {
          println("here")
          Node("", "")
        } // Should never get here.
        case Some(v) => v
      }
      path(i) match {
        case 'L' => current = node.left
        case 'R' => current = node.right
      }
      counter += 1
      // if (steps.filter(_.matches("..Z")).length == steps.length) {
      // println(steps)
      // break(())
      // }
      if (current.last == 'Z') {
        break(())
      }
      i += 1
    }

  counter
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

