import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

@main def parse(input: String): Unit = {
  val path: os.Path = os.pwd / input
  val lines: Seq[String] = os.read.lines(path)
  println(parseCards(lines))
}

val map: Map[Card, Int] = Map(
  (Card.One, 0),
  (Card.Two, 0),
  (Card.Three, 0),
  (Card.Four, 0),
  (Card.Five, 0),
  (Card.Six, 0),
  (Card.Seven, 0),
  (Card.Eight, 0),
  (Card.Nine, 0),
  (Card.Ten, 0),
  (Card.Prince, 0),
  (Card.Queen, 0),
  (Card.King, 0),
  (Card.Ace, 0)
  )

enum Card {
  case One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Prince, Queen, King, Ace
}

enum HandType { 
  case HighCard, OnePair, TwoPair, ThreeOrAKind, FullHouse, FourOfAKind, FiveOfAKind
}

object Hand {
  def create(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card, bid: Int): Hand = {
    val temp: Map[Card, Int] = map.clone
    val list = List(c1, c2, c3, c4, c5)

    list.map(x => temp.get(x) match {
      case None => ()
      case Some(value) => {
        temp.addOne((x, value + 1))
        ()
      }
    })

    val x = temp.values.toList.filterNot(v => v == 0)
    Hand( 
    {
      if (x.length == 1) {HandType.FiveOfAKind}
      else if (x.length == 5) {HandType.HighCard}
      else if (x.length == 4) {HandType.OnePair}
      else if (x.length == 3 && x.contains(2)) {HandType.TwoPair}
      else if (x.length == 3 && x.contains(3)) {HandType.ThreeOrAKind}
      else if (x.length == 2 && x.contains(3)) {HandType.FullHouse}
      else if (x.length == 2 && x.contains(4)) {HandType.FourOfAKind}
      else {HandType.FiveOfAKind}
    },
    c1,c2,c3,c4,c5,bid)
  }

  def greater(h1: Hand, h2: Hand): Boolean = {
    if      (h1.h.ordinal > h2.h.ordinal) {true}
    else if (h1.h.ordinal < h2.h.ordinal) {false}
    else if (h1.c1.ordinal > h2.c1.ordinal) {true}
    else if (h1.c1.ordinal < h2.c1.ordinal) {false}
    else if (h1.c2.ordinal > h2.c2.ordinal) {true}
    else if (h1.c2.ordinal < h2.c2.ordinal) {false}
    else if (h1.c3.ordinal > h2.c3.ordinal) {true}
    else if (h1.c3.ordinal < h2.c3.ordinal) {false}
    else if (h1.c4.ordinal > h2.c4.ordinal) {true}
    else if (h1.c4.ordinal < h2.c4.ordinal) {false}
    else if (h1.c5.ordinal > h2.c5.ordinal) {true}
    else {false}
  }
}

case class Hand(h: HandType, c1: Card, c2: Card, c3: Card, c4: Card, c5: Card, bet: Int)

def parseCards(lines: Seq[String]): Long = {
  var array: ArrayBuffer[Hand] = ArrayBuffer()
  for line <- lines do {
    val cards = line.split(' ')(0)
    val bid = line.split(' ')(1).trim.toInt
    val hand = parseCard(cards, bid)
    array.addOne(hand)
  }
  array = array.sortWith((h1, h2) => Hand.greater(h1,h2))
  var total = 0
  for i <- 0 to array.length - 1 do {
    total += array(i).bet * (array.length - i)
  }
  array.map(println(_))
  total
}

def parseCard(cards: String, bid: Int): Hand = {
  val list: Array[Card] = cards
    .split("")
    .map(_ match {
      case "1" => Card.One
      case "2" => Card.Two
      case "3" => Card.Three
      case "4" => Card.Four
      case "5" => Card.Five
      case "6" => Card.Six
      case "7" => Card.Seven
      case "8" => Card.Eight
      case "9" => Card.Nine
      case "T" => Card.Ten
      case "J" => Card.Prince
      case "Q" => Card.Queen
      case "K" => Card.King
      case  _  => Card.Ace
    })

  Hand.create(list(0), list(1), list(2), list(3), list(4), bid)
}


