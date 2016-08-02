import scala.util.Random

object poker {
  case class Card(number: Int, color: Int) {
    require(number > 1 && number < 15, "Incorrect value of card")
    require(color > 0 && color < 5, "Incorrect color of card")
  }

  def randomCard: List[Card] = {
    val x = new Random
    val y = new Random
    val cards = List(new Card(x.nextInt(13)+2, y.nextInt(4)+1), new Card(x.nextInt(13)+2, y.nextInt(4)+1), new Card(x.nextInt(13)+2, y.nextInt(4)+1), new Card(x.nextInt(13)+2, y.nextInt(4)+1), new Card(x.nextInt(13)+2, y.nextInt(4)+1))
    if(List(cards.count(_ == cards(0)),cards.count(_ == cards(1)),cards.count(_ == cards(2)),cards.count(_ == cards(3)),cards.count(_ == cards(4))).max > 1) randomCard
    else cards
  }

  def checkHighCard(list: List[Card]): Int = {
    var highCard: Int = 0
    for(x <- 0 to 4){
      if(list(x).number > highCard) highCard = list(x).number
    }
    highCard
  }

  def checkPair(list: List[Card]): Int = {
    val value = List(list.count(_ == list(0)),list.count(_ == list(1)),list.count(_ == list(2)),list.count(_ == list(3)),list.count(_ == list(4)))
    val value2 = List(value.count(_ == value(0)),value.count(_ == value(1)),value.count(_ == list(2)),value.count(_ == value(3)),value.count(_ == value(4)))
    if (value2.max == 4) 5
    else value.max
  }

  def checkStraight(list: List[Card]): Boolean = {
    (list(0).number == list(1).number - 1 && list(0).number == list(2).number - 2 &&list(0).number == list(3).number - 3 && list(0).number == list(4).number - 4) ||
      (list(0).number == list(1).number + 1 && list(0).number == list(2).number + 2 && list(0).number == list(3).number + 3 && list(0).number == list(4).number + 4)
  }

  def checkColor(list: List[Card]): Boolean = {
    list(0).color == list(1).color && list(0).color == list(2).color && list(0).color == list(3).color && list(0).color == list(4).color

  }

  def checkFullHouse(list: List[Card]): Boolean = {
    val value = List(list.count(_ == list(0)),list.count(_ == list(1)),list.count(_ == list(2)),list.count(_ == list(3)),list.count(_ == list(4)))
    value.max == 3 && value.count(_ == 2) == 2
  }

  def checkPoker(list: List[Card]): Boolean = {
    checkColor(list) && checkStraight(list)
  }

  def check(list: List[Card]): String = {
    if(checkPoker(list)) "Straight flush!"
    else if(checkPair(list) == 4) "Four of kind"
    else if(checkFullHouse(list)) "Full house"
    else if(checkColor(list)) "Flush"
    else if(checkStraight(list)) "Straight"
    else if(checkPair(list) == 3) "Three of kind"
    else if(checkPair(list) == 5) "Two pair"
    else if(checkPair(list) == 2) "One pair"
    else {
      if(checkHighCard(list) == 14) "High Card: Ace"
      else if(checkHighCard(list) == 13) "High Card: King"
      else if(checkHighCard(list) == 12) "High Card: Queen"
      else if(checkHighCard(list) == 11) "High Card: Jack"
      else "High Card: " + checkHighCard(list).toString()
    }
  }

  def main(args: Array[String]): Unit ={
    val card: List[Card] = randomCard
    val card2: List[Card] = List(new Card(10,2),new Card(9,2), new Card(8,2), new Card(7,2), new Card(6, 2))

    println(check(card))
    println(check(card2))
  }

}
