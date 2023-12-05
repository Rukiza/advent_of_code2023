import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.boundary, boundary.break

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)

  println(parseMaps(lines))
}

def parseMaps(lines: Seq[String]): Long = {
  val (seeds: List[Long], rest1: Seq[String]) = readSeedLines(lines)
  val (seed_to_soil, rest2) = readMaps(rest1)
  val (soil_to_fertilizer, rest3) = readMaps(rest2)
  val (fertilizer_to_water, rest4) = readMaps(rest3)
  val (water_to_light, rest5) = readMaps(rest4)
  val (light_to_temperature, rest6) = readMaps(rest5)
  val (temperature_to_humidity, rest7) = readMaps(rest6)
  val (humidity_to_location, rest8) = readMaps(rest7)
  seeds
    .map(x => getValue(seed_to_soil, x))
    .map(x => getValue(soil_to_fertilizer,x))
    .map(x => getValue(fertilizer_to_water,x))
    .map(x => getValue(water_to_light,x))
    .map(x => getValue(light_to_temperature,x))
    .map(x => getValue(temperature_to_humidity,x))
    .map(x => getValue(humidity_to_location,x))
    .foldLeft(Long.MaxValue)((acc, x) => {
      if (acc < x) {
       acc
    } else {
      x
    }})
}

def readSeedLines(lines: Seq[String]): (List[Long], Seq[String]) = {
  val seeds: Array[Long] = lines
    .takeWhile(_ != "")
    .foldLeft("")((acc, h) => acc + h)
    .split(":")(1)
    .trim
    .split(" ")
    .map(_.toLong)
  var rest = lines.dropWhile(_ != "").drop(1)
  (seeds.toList, rest)
}

def readMaps(lines: Seq[String]): (List[Array[Long]], Seq[String]) = {
  println(lines)
  val map = lines
    .drop(1)
    .takeWhile(_ != "")
    .map(_.trim)
    .map(_.split(" "))
    .map(_.map(_.toLong))
  var rest = lines.dropWhile(_ != "").drop(1)
  (map.toList, rest)
}

def getValue(map: List[Array[Long]], value: Long): Long = {
  boundary:
    for array <- map do {
      if (value >= array(1) 
        && value <= array(1) + array(2)) {
          val out: Long = array(0) + (array(2) - (array(1) + array(2) - value))
          break(out)
      }
    } 
    value
}


