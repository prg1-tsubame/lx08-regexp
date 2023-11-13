package prg1.lx09.traits

trait Suit

object Ex1aSimple {
    object Club extends Suit
    object Spade extends Suit
    object Diamond extends Suit
    object Heart extends Suit

    def run = {
        val suits = List[Suit](Spade, Heart)
        for (suit <- suits) println(suit)
    }
}

object Ex1bSimple {  // object => case object と変更しただけ
    case object Club extends Suit    // ♣️
    case object Spade extends Suit    // ♠️
    case object Diamond extends Suit    // ♦️
    case object Heart extends Suit    // ❤️

    def run = {
        val suits = List[Suit](Spade, Heart)
        for (suit <- suits) println(suit)
    }
}

@main def ex1_simple_trait = {
    Ex1aSimple.run
    println(" ")
    Ex1bSimple.run
}


object Cards {
    import Ex1bSimple._  // object Ex1bSimple の内側の定義をすべて、この object に読み込む

    trait Rank

    case object Ace   extends Rank
    case class  Number(n: Int) extends Rank
    case object Jack  extends Rank
    case object Queen extends Rank
    case object King  extends Rank

    case class Card(suit: Suit, rank: Rank)

    def run = {
        val cards = List[Card](
            Card(Spade,   Ace),
            Card(Heart,   Ace),
            Card(Club,    Number(3)),
            Card(Diamond, Number(3)),
            Card(Heart,   Number(3)))

        for (card <- cards) println(card)
    }
}

@main def ex2a_parametric_cards = Cards.run

object Pets {
    trait Pet {
        val name: String
    }

    class Cat(val name: String) extends Pet

    class Dog(val name: String) extends Pet

    def run = {
        val dog = Dog("ぽち")
        val cat1 = Cat("たま")
        val cat2 = Cat("三毛")

        import scala.collection.mutable.ArrayBuffer

        val animals = ArrayBuffer[Pet](dog, cat1, cat2, Cat("名前はまだ無い"))
        animals.foreach(pet => println((pet.name, pet)))
    }
}

@main def ex2b_parametric_pets = Pets.run
