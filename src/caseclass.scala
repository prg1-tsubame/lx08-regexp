package prg1.lx08.caseclass

object ClassExample {
    class Person(first: String, last: String)

    def run() = {
        println("Class Test")

        val p1 = new Person("Ken", "Wakita")
        val p2 = new Person("Mikito", "Nanashima")

        val people = List[Person](p1, p2)
        for (person <- people) {
            println(person)
        }
    }
}

object CaseClassExample {
    case class Person(first: String, last: String)

    def run() = {
        println(" \nCase Class Test")
        
        val p1 = Person("Ken", "Wakita")
        val p2 = Person("Mikito", "Nanashima")

        val people = List[Person](p1, p2)
        for (person <- people) {
            println(person)
            person match {
                case Person(first, _) => println(first)
            }
        }
    }
}
@main def ex_caseclass1 = {
    ClassExample.run()
    CaseClassExample.run()
}
