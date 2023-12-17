import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable
import scala.util.boundary, boundary.break

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines(lines))
}

enum Pipe {
  case V,H,NE,NW,SW,SE,G,S
}

enum Direction {
  case R, L
}

enum Travel {
  case N, S, E, W
}

case class Node(r: Int, c: Int)

def parseLines(lines: Seq[String]): BigInt = {
  var map = lines.map(_.toList.map(_ match {
    case '|' => Pipe.V
    case '-' => Pipe.H
    case 'L' => Pipe.NE
    case 'J' => Pipe.NW
    case '7' => Pipe.SW
    case 'F' => Pipe.SE
    case '.' => Pipe.G
    case 'S' => Pipe.S
  })).toList
  map.map(println(_))
  var start = Node(0, 0)
  boundary:
    for r <- 0 to map.length -1 do {
      for c <- 0 to map(r).length -1 do {
        if (map(r)(c) == Pipe.S) {
          start = Node(r,c)
          break(())
        }
      }
    }

  var path: ArrayBuffer[ArrayBuffer[Node]] = ArrayBuffer(ArrayBuffer(start))

  path = calculatePath(path, map) 

  println(calculateInner(path(0), map))

  println(path(0).length/2.ceil.toInt)
  0
}

def calculatePath(
  path: ArrayBuffer[ArrayBuffer[Node]], 
  map: List[List[Pipe]]
): ArrayBuffer[ArrayBuffer[Node]] = {

  calculatePath(path,map, 0, path(0)(0).r, path(0)(0).c)

}

def calculateInner(
  path: ArrayBuffer[Node],
  map: List[List[Pipe]]
): Int = {
  val innerEdge = findEdgeSlide(path, map) match {
    case Direction.R => Direction.L
    case Direction.L => Direction.R
  }

  // println(innerEdge)
  var ralitive = Travel.N
  var markedNodes: ArrayBuffer[Node] = ArrayBuffer()
  for i <- 1 to path.length - 1 do {
    val prev = path(i-1)
    val curr = path(i)

    val rDir = prev.r - curr.r
    val cDir = prev.c - curr.c
    println(curr)
    println(map(curr.r)(curr.c))
    println("rDir: " + rDir)
    println("cDir: " + cDir)

    if (rDir > 0 && map(curr.r)(curr.c) == Pipe.V) {
      ralitive = Travel.N
      val n = if (innerEdge == Direction.L) {
        Node(curr.r, curr.c-1)
      }
      else {
        Node(curr.r, curr.c+1)
      }
      
      if (!markedNodes.contains(n) && !path.contains(n) && n.r >= 0 && n.r < map.length && n.c >= 0 && n.c < map(n.r).length) {
        markedNodes.addOne(n)
      }
    }
    else if (rDir < 0 && map(curr.r)(curr.c) == Pipe.V || (map(curr.r)(curr.c) == Pipe.SW || map(curr.r)(curr.c) == Pipe.SE)) {
      ralitive = Travel.S
      val n = if (innerEdge == Direction.L) {
        Node(curr.r, curr.c+1)
      }
      else {
        Node(curr.r, curr.c-1)
      }
      if (!markedNodes.contains(n) && !path.contains(n) && n.r >= 0 && n.r < map.length && n.c >= 0 && n.c < map(n.r).length) {
        markedNodes.addOne(n)
      }
    }
    else if (cDir > 0 && (map(curr.r)(curr.c) == Pipe.H || map(curr.r)(curr.c) == Pipe.NW)) {
      ralitive = Travel.W
      val n = if (innerEdge == Direction.L) {
        Node(curr.r+1, curr.c)
      }
      else {
        Node(curr.r-1, curr.c)
      }
      if (!markedNodes.contains(n) && !path.contains(n) && n.r >= 0 && n.r < map.length && n.c >= 0 && n.c < map(n.r).length) {
        markedNodes.addOne(n)
      }
    }
    else if (cDir < 0 && map(curr.r)(curr.c) == Pipe.H) {
      ralitive = Travel.E
      val n = if (innerEdge == Direction.L) {
        Node(curr.r-1, curr.c)
      }
      else {
        Node(curr.r+1, curr.c)
      }
      if (!markedNodes.contains(n) && !path.contains(n) && n.r >= 0 && n.r < map.length && n.c >= 0 && n.c < map(n.r).length) {
        markedNodes.addOne(n)
      }
    }
    println(ralitive)
    
  }
  
  markedNodes.length
}

def findEdgeSlide(path: ArrayBuffer[Node], map: List[List[Pipe]]): Direction = {
  val temp = path.map(x => map(x.r)(x.c) match {
    // case Pipe.H => check(lookNorth, lookSouth, x.r, x.c, path, map)
    // case Pipe.V => check(lookEast, lookWest, x.r, x.c, path, map)
    case Pipe.NW if (lookEast(x.r, x.c,path,map) || lookSouth(x.r,x.c,path,map)) => Some(Direction.R)
    case Pipe.NE if (lookWest(x.r, x.c,path,map) || lookSouth(x.r,x.c,path,map)) => Some(Direction.R)
    case Pipe.SE if (lookWest(x.r, x.c,path,map) || lookNorth(x.r,x.c,path,map)) => Some(Direction.R)
    case Pipe.SW if (lookEast(x.r, x.c,path,map) || lookNorth(x.r,x.c,path,map)) => Some(Direction.R)
    case _ => None
  }).flatMap(x => x)

  if (temp.isEmpty) {
    Direction.L
  }
  else {
    Direction.R
  }
}

def check(
  f1: (Int, Int, ArrayBuffer[Node], List[List[Pipe]]) => Boolean, 
  f2: (Int, Int, ArrayBuffer[Node], List[List[Pipe]]) => Boolean, 
  r: Int,
  c: Int,
  path: ArrayBuffer[Node],
  map: List[List[Pipe]]
): Option[Direction] = {
  if (f1(r,c,path,map)) {
    Some(Direction.R)
  }
  else if (f2(r,c,path,map)) {
    Some(Direction.L)
  }
  else {
    None
  }
}

def lookNorth(r: Int, c: Int, path: ArrayBuffer[Node], map: List[List[Pipe]]): Boolean = {
  var notFound = true
  for i <- 0 to r - 1 do {
    if (path.contains(Node(i,c))) {
      notFound = false
    }
  }
  notFound
}

def lookSouth(r: Int, c: Int, path: ArrayBuffer[Node], map: List[List[Pipe]]): Boolean = {
  var notFound = true
  for i <- r + 1 to map.length - 1 do {
    if (path.contains(Node(i,c))) {
      notFound = false
    }
  }
  notFound
}

def lookEast(r: Int, c: Int, path: ArrayBuffer[Node], map: List[List[Pipe]]): Boolean = {
  var notFound = true
  for i <- c + 1 to map(r).length - 1 do {
    if (path.contains(Node(r,i))) {
      notFound = false
    }
  }
  notFound
}

def lookWest(r: Int, c: Int, path: ArrayBuffer[Node], map: List[List[Pipe]]): Boolean = {
  var notFound = true
  for i <- 0 to c - 1 do {
    if (path.contains(Node(r,i))) {
      notFound = false
    }
  }
  notFound
}

def calculatePath(
  path: ArrayBuffer[ArrayBuffer[Node]], 
  map: List[List[Pipe]],
  layer: Int,
  r: Int, 
  c:Int
): ArrayBuffer[ArrayBuffer[Node]] = {
  
  if (r >= map.length) { path }
  else if (c >= map(r).length) { path }
  else {
    map(r)(c) match {
      case Pipe.S => {
        val nodes: List[Node] = List(
          Node(r-1,c),
          Node(r,c-1),
          Node(r+1, c),
          Node(r, c+1)
        )
        if (path(layer).length > 1) {
          println("help me ")
          path
        }
        else {
          val newNodes: List[Node] = nodes
            .filter( _ match {case Node(r,c) => r >= 0 && c >= 0})
            .map(_ match {
            case Node(r1, c1) => (map(r1)(c1), r1, c1)
          }).map({
            _ match {
              case (Pipe.V, (r1), c1) if r1 == r+1 || r1 == r-1 => Option(Node(r1, c1))
              case (Pipe.H, (r1), c1) if c1 == c+1 || c1 == c-1 => Option(Node(r1, c1))
              case (Pipe.NE, (r1), c1) if r1 == r+1 || c1 == c-1 => Option(Node(r1, c1))
              case (Pipe.NW, (r1), c1) if r1 == r+1 || c1 == c+1 => Option(Node(r1, c1))
              case (Pipe.SE, (r1), c1) if r1 == r-1 || c1 == c-1 => Option(Node(r1, c1))
              case (Pipe.SW, (r1), c1) if r1 == r-1 || c1 == c+1 => Option(Node(r1, c1))
              case (_, _, _) => None
            }
          }).flatMap(x => x)

          println("NewNodes: "+newNodes)
          println("did this bit worl")
          val temp = path(layer).clone
          path(layer).addOne(newNodes(0))
          temp.addOne(newNodes(0))
          path.addOne(temp)
          // val newPath = 
            calculatePath(
            path,
            map,
            layer,
            newNodes(0).r,
            newNodes(0).c
          )
          // calculatePath(
            // newPath,
            // map,
            // layer+1,
            // newNodes(1).r,
            // newNodes(1).c
          
        }
      }
      case Pipe.G => { path } // We really have run into a problem
      case Pipe.H => {
        println("horizontal")
        val prev = path(layer)(path(layer).length - 2)
        println(s"r: ${r}, c: ${c}, dif-r: ${0}, dif-c: ${c - prev.c}")
        path(layer).addOne(Node(
          r,
          c + (c - prev.c)
        ))
        calculatePath(
          path,
          map,
          layer,
          r,
          c + (c - prev.c)
        )
      }
      case Pipe.V => {
        println("vertical")
        val prev = path(layer)(path(layer).length - 2)
        println(s"r: ${r}, c: ${c}, dif-r: ${r- prev.r}, dif-c: ${0}")
        path(layer).addOne(Node(
          r + (r - prev.r),
          c
        ))
        calculatePath(
          path,
          map,
          layer,
          r + (r - prev.r),
          c 
        )
      }
      case Pipe.NE => {
        println("north east")
        val prev = path(layer)(path(layer).length - 2)
        println(s"r: ${r}, c: ${c}, dif-r: ${c - prev.c}, dif-c: ${r - prev.r}")
        path(layer).addOne(Node(
          r + (c - prev.c),
          c + (r - prev.r)
        ))
        calculatePath(
          path,
          map,
          layer,
          r + (c - prev.c),
          c + (r - prev.r)
        )
      }
      case Pipe.SE => {
        val prev = path(layer)(path(layer).length - 2)
        println(s"south east r: ${r}, c: ${c}, dif-r: ${prev.c - c}, dif-c: ${prev.r - r}")
        path(layer).addOne(Node(
          r + (prev.c - c),
          c + (prev.r - r)
        ))
        calculatePath(
          path,
          map,
          layer,
          r + (prev.c - c),
          c + (prev.r - r)
        )
      }
      case Pipe.NW => {
        println("north west")
        val prev = path(layer)(path(layer).length - 2)
        println(s"r: ${r}, c: ${c}, dif-r: ${prev.c - c}, dif-c: ${prev.r - r}")
        path(layer).addOne(Node(
          r + (prev.c - c),
          c + (prev.r - r)
        ))
        calculatePath(
          path,
          map,
          layer,
          r + (prev.c - c),
          c + (prev.r - r)
        )
      }
      case Pipe.SW => {
        println("south west")
        val prev = path(layer)(path(layer).length - 2)
        println(s"r: ${r}, c: ${c}, dif-r: ${c - prev.c}, dif-c: ${r - prev.r}")
        path(layer).addOne(Node(
          r + (c - prev.c),
          c + (r - prev.r)
        ))
        calculatePath(
          path,
          map,
          layer,
          r + (c - prev.c),
          c + (r - prev.r)
        )
      }
    }
  }

}
