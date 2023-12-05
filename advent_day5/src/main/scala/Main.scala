import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.boundary, boundary.break
import scala.collection.immutable.NumericRange

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)

  println(parseMaps(lines))
}

def parseMaps(lines: Seq[String]): Long = {
  val (seeds: List[NumericRange.Inclusive[Long]], rest1: Seq[String]) = readSeedLines(lines)
  val (seed_to_soil, rest2) = readMaps(rest1)
  val (soil_to_fertilizer, rest3) = readMaps(rest2)
  val (fertilizer_to_water, rest4) = readMaps(rest3)
  val (water_to_light, rest5) = readMaps(rest4)
  val (light_to_temperature, rest6) = readMaps(rest5)
  val (temperature_to_humidity, rest7) = readMaps(rest6)
  val (humidity_to_location, rest8) = readMaps(rest7)
  // var out: ArrayBuffer[Long] = ArrayBuffer()

  var out: Long = Long.MaxValue
  for seedRange <- seeds
  x <- seedRange do {
    val t1 = getValue(seed_to_soil, x)
    val t2 = getValue(soil_to_fertilizer,t1)
    val t3 = getValue(fertilizer_to_water,t2)
    val t4 = getValue(water_to_light,t3)
    val t5 = getValue(light_to_temperature,t4)
    val t6 = getValue(temperature_to_humidity,t5)
    val t7 = getValue(humidity_to_location,t6)
    out = if (out < t7) {
      out
    } else {
      t7
    }
  }
  out
}

def readSeedLines(lines: Seq[String]): (List[NumericRange.Inclusive[Long]], Seq[String]) = {
  val seeds: Array[Long] = lines
    .takeWhile(_ != "")
    .foldLeft(" ")((acc, h) => acc + h)
    .split(":")(1)
    .trim
    .split(" ")
    .map(_.toLong)
    var rest = lines.dropWhile(_ != "").drop(1)

    var i = 0 
    var out: ArrayBuffer[NumericRange.Inclusive[Long]] = ArrayBuffer()
    while (i < seeds.length) {
      val start = seeds(i)
      val amount = seeds(i+1)
      println(s"start ${start} amount ${amount}")
      out.addOne((start to start + amount-1))
      i += 2
    }

  (out.toList, rest)
}

def readMaps(lines: Seq[String]): (Array[Array[Long]], Seq[String]) = {
  val map = lines
    .drop(1)
    .takeWhile(_ != "")
    .map(_.trim)
    .map(_.split(" "))
    .map(_.map(_.toLong))
  var rest = lines.dropWhile(_ != "").drop(1)
  (map.toArray, rest)
}

def getValue(map: Array[Array[Long]], value: Long): Long = {
  boundary:
    for array <- map do {
      if (value >= array(1) 
        && value < array(1) + array(2)) {
          val out: Long = array(0) + (array(2) - (array(1) + array(2) - value))
          break(out)
      }
    } 
    value
}


