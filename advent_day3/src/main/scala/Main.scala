

@main def parse(input: String): Unit = {

  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  lines.map(println)

}

