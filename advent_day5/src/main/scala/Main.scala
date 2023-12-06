import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.boundary, boundary.break
import scala.collection.immutable.NumericRange
import scala.collection.mutable.Stack

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)

  println(parseMaps(lines))

}

case class SeedRange(start: Long, end: Long)

case class RangeFilter(destination: Long, source: Long, range: Long)

def filterByRange(seedRange: SeedRange, rangeFilter: RangeFilter): (Option[SeedRange], Option[SeedRange], Option[SeedRange]) = {
  // seed is within range. 
  if (seedRange.start >= rangeFilter.source && 
    seedRange.end < rangeFilter.source + rangeFilter.range) {
    val center = seedRange.copy(
      start = rangeFilter.destination + (rangeFilter.range - (rangeFilter.source + rangeFilter.range - seedRange.start)),
      end = rangeFilter.destination + (rangeFilter.range - (rangeFilter.source + rangeFilter.range - seedRange.end)))
    (None, Some(center), None)
  }
  else if (seedRange.start < rangeFilter.source && 
    seedRange.end >= rangeFilter.source + rangeFilter.range) {
    val left = SeedRange(seedRange.start, rangeFilter.source - 1)
    val right = SeedRange(rangeFilter.source + rangeFilter.range, seedRange.end)
    val center = SeedRange(rangeFilter.destination, rangeFilter.destination + rangeFilter.range - 1)
    (Some(left), Some(center), Some(right))
  }
  else if (seedRange.start < rangeFilter.source && 
    seedRange.end < rangeFilter.source + rangeFilter.range &&
    seedRange.end > rangeFilter.source) {
    val left = SeedRange(seedRange.start, rangeFilter.source - 1)
    val center = seedRange.copy(
      start = rangeFilter.destination, 
      end = rangeFilter.destination + (rangeFilter.range - (rangeFilter.source + rangeFilter.range - seedRange.end)))
    (Some(left), Some(center), None)
  }
  else if (seedRange.start >= rangeFilter.source && 
    seedRange.end >= rangeFilter.source + rangeFilter.range &&
    seedRange.start < rangeFilter.source + rangeFilter.range) {
    val right = SeedRange(rangeFilter.source + rangeFilter.range, seedRange.end)
    val center = seedRange.copy(
      start = rangeFilter.destination + (rangeFilter.range - (rangeFilter.source + rangeFilter.range - seedRange.start)),
      end = rangeFilter.destination + rangeFilter.range
    )
    (None, Some(center), Some(right))
  }
  else {
    (Some(seedRange), None, None)
  }
}

def parseMaps(lines: Seq[String]): Long = {
  val (seeds: List[SeedRange], rest1: Seq[String]) = readSeedLines(lines)
  val (seed_to_soil, rest2) = readMaps(rest1)
  val (soil_to_fertilizer, rest3) = readMaps(rest2)
  val (fertilizer_to_water, rest4) = readMaps(rest3)
  val (water_to_light, rest5) = readMaps(rest4)
  val (light_to_temperature, rest6) = readMaps(rest5)
  val (temperature_to_humidity, rest7) = readMaps(rest6)
  val (humidity_to_location, rest8) = readMaps(rest7)

  seeds
    .flatMap(x => getValue(seed_to_soil, x))
    .flatMap(x => getValue(soil_to_fertilizer, x))
    .flatMap(x => getValue(fertilizer_to_water, x))
    .flatMap(x => getValue(water_to_light, x))
    .flatMap(x => getValue(light_to_temperature, x))
    .flatMap(x => getValue(temperature_to_humidity, x))
    .flatMap(x => getValue(humidity_to_location, x))
    .fold(SeedRange(Long.MaxValue, 0))( (out, x) => 
      if (out.start < x.start) {
        out
      } else {
        x
      }
    ).start
}

def readSeedLines(lines: Seq[String]): (List[SeedRange], Seq[String]) = {
  val seeds: Array[Long] = lines
    .takeWhile(_ != "")
    .foldLeft(" ")((acc, h) => acc + h)
    .split(":")(1)
    .trim
    .split(" ")
    .map(_.toLong)
    var rest = lines.dropWhile(_ != "").drop(1)

    var i = 0 
    var out: ArrayBuffer[SeedRange] = ArrayBuffer()
    while (i < seeds.length) {
      val start = seeds(i)
      val amount = seeds(i+1)
      println(s"start ${start} amount ${amount}")
      out.addOne(SeedRange(start, start + amount-1))
      i += 2
    }

  (out.toList, rest)
}

def readMaps(lines: Seq[String]): (Array[RangeFilter], Seq[String]) = {
  val map = lines
    .drop(1)
    .takeWhile(_ != "")
    .map(_.trim)
    .map(_.split(" "))
    .map(_.map(_.toLong))
    .map(x => RangeFilter(x(0),x(1),x(2)))
  var rest = lines.dropWhile(_ != "").drop(1)
  (map.toArray, rest)
}

def getValue(map: Array[RangeFilter], seed: SeedRange): Array[SeedRange] = {
  var stack = Stack[Option[SeedRange]](Some(seed))
  var array: ArrayBuffer[Option[SeedRange]] = ArrayBuffer()
  for m <- map do {
    var toProcess: Stack[Option[SeedRange]] = stack.clone()
    stack = stack.empty
    while(!toProcess.isEmpty) {
      toProcess.pop match {
        case None => ()
        case Some(value) => {
          val (a,b,c) = filterByRange(value, m)
          stack.push(a)
          array.addOne(b) 
          stack.push(c)
        }
      }
    }
  }
  array.addAll(stack).flatMap(x => x).toArray
}


