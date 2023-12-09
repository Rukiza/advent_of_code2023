import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseLines2(lines))
}

def parseLines(lines: Seq[String]): BigInt = {
  var list: ArrayBuffer[Pyramid] = ArrayBuffer()
  for line <- lines do {
    var p: Pyramid = Pyramid()
    line
      .split(" ")
      .map(_.toLong)
      .foreach(p.add(_))
    list.addOne(p)
  }

  val n = list.map(_.calcNext).reduce(_ + _)

  n 
}

def parseLines2(lines: Seq[String]): BigInt = {
  var list: ArrayBuffer[Pyramid] = ArrayBuffer()
  for line <- lines do {
    var p: Pyramid = Pyramid()
    line
      .split(" ")
      .map(_.toLong)
      .foreach(p.add(_))
    list.addOne(p)
  }

  println(list.length)

  val n = list.map(_.calcPrev).reduce(_ + _)

  // list.map(println(_))

  n 
}

class Pyramid {

  // Init layers array with buffer containing 0
  var layers: ArrayBuffer[ArrayBuffer[BigInt]] = ArrayBuffer()

  def add(value: BigInt): Unit = {
    if (layers.isEmpty) {
      layers.addOne(ArrayBuffer(value))
    }
    else {
      layers(0).addOne(value)
      calcLevel(1)
    }
  }

  def calcNext: BigInt = {
    calcNext(0)
  }

  def calcPrev: BigInt = {
    calcPrev(0)
  }

  private def calcNext(level: Int): BigInt = {
    if (level == layers.length - 1) {
      layers(level).addOne(0)
      0
    }
    else {
      val dif = calcNext(level + 1)
      val value = dif + layers(level).last
      layers(level).addOne(value)
      value
    }
  }

  private def calcPrev(level: Int): BigInt = {
    if (level == layers.length - 1) {
      layers(level) = ArrayBuffer(BigInt(0)) ++ layers(level)
      0
    }
    else {
      val dif = calcPrev(level + 1)
      val value: BigInt = layers(level)(0) - dif
      layers(level) = ArrayBuffer[BigInt](value) ++ layers(level)
      value
    }
  }

  private def calcLevel(level: Int): Unit = {
    if (layers.length == level) {
      layers.addOne(ArrayBuffer())
    }

    // var dif: BigInt = 0
    // if (layers(level).length == 0)
    // {
    val  dif = layers(level-1)(layers(level).length+1) - layers(level-1)(layers(level).length)
    // }
    // else {
      // dif = layers(level-1)(layers(level - 1).length-1) - layers(level-1)(layers(level-1).length-2)
    // }

    layers(level).addOne(dif)

    if (//layers(level).filter(_ != 0).length != 0 && 
      layers(level).length > 1) {

      calcLevel(level+1)
    }
  }

  override def toString(): String = {
    // var str: mutable.StringBuilder = mutable.StringBuilder()
    // var tabCount = 0
    // for l <- layers do {
    //
    //   for i <- 1 to tabCount do {
    //     str += '\t'
    //   }
    //
    //   for v <- l do {
    //     str ++= s"${v}\t\t"
    //   }
    //
    //   str += '\n'
    //
    //   tabCount += 1
    // }
    // str.toString()
    layers.toString()
  }
}
