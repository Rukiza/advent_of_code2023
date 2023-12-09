

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  test("test line 1") {
    val lines = Seq("14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54")
    assert(parseLines(lines) ==  56) 
  }

  test("test line 2") {
    val lines = Seq("0 -5 -10 -15 -20 -25 -30 -35 -40 -45 -50 -55 -60 -65 -70 -75 -80 -85 -90 -95 -100")
    assert(parseLines(lines) ==  -105) 
  }

  test("test line 3") {
    val lines = Seq("7 18 33 54 79 98 89 14 -185 -590 -1311 -2490 -4305 -6974 -10759 -15970 -22969 -32174 -44063 -59178 -78129")
    assert(parseLines(lines) ==  -101598) 
  }

  test("test lines 1") {
    val lines = Seq(
      "7 18 33 54 79 98 89 14 -185 -590 -1311 -2490 -4305 -6974 -10759 -15970 -22969 -32174 -44063 -59178 -78129",
      "0 -5 -10 -15 -20 -25 -30 -35 -40 -45 -50 -55 -60 -65 -70 -75 -80 -85 -90 -95 -100"
    )
    assert(parseLines(lines) ==  -101703) 
  }

  test("test lines 2") {
    val lines = Seq(
      "14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54",
      "0 -5 -10 -15 -20 -25 -30 -35 -40 -45 -50 -55 -60 -65 -70 -75 -80 -85 -90 -95 -100"
    )
    assert(parseLines(lines) ==  -49) 
  }

  
}
