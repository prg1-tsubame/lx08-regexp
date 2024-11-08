package prg1.lx09.traits

trait Suit

object Ex1aSimple {
    object Club extends Suit
    object Spade extends Suit
    object Diamond extends Suit
    object Heart extends Suit

    @main def run_1a = {
        val suits = List[Suit](Spade, Heart)
        println("The list of cards")
        for (suit <- suits) println(suit)
    }
}

object Ex1bSimple {  // object => case object と変更しただけ
    case object Club extends Suit    // ♣️
    case object Spade extends Suit    // ♠️
    case object Diamond extends Suit    // ♦️
    case object Heart extends Suit    // ❤️

    @main def run_1b = {
        val suits = List[Suit](Spade, Heart)
        println("The list of cards")
        for (suit <- suits) println(suit)
    }
}

object Cards {
    import Ex1bSimple.*  // object Ex1bSimple の内側の定義をすべて、この object に読み込む

    trait Rank

    case object Ace   extends Rank
    case class  Number(n: Int) extends Rank
    case object Jack  extends Rank
    case object Queen extends Rank
    case object King  extends Rank

    case class Card(suit: Suit, rank: Rank)

    @main def run_cards = {
        val cards = List[Card](
            Card(Spade,   Ace),
            Card(Heart,   Ace),
            Card(Club,    Number(3)),
            Card(Diamond, Number(3)),
            Card(Heart,   Number(3)))

        for (card <- cards) println(card)
    }
}

object Pets {
    trait Pet {
        val name: String
    }

    class Cat(val name: String) extends Pet

    class Dog(val name: String) extends Pet

    @main def run_pets = {
        val dog = Dog("ぽち")
        val cat1 = Cat("たま")
        val cat2 = Cat("三毛")

        import scala.collection.mutable.ArrayBuffer

        val animals = ArrayBuffer[Pet](dog, cat1, cat2, Cat("名前はまだ無い"))
        animals.foreach(pet => println((pet.name, pet)))
    }
}
