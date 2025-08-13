object ZebraPuzzle {

  sealed trait Resident

  case object Englishman extends Resident
  case object Spaniard extends Resident
  case object Ukrainian extends Resident
  case object Norwegian extends Resident
  case object Japanese extends Resident

  case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  lazy val solve: Solution = solver().next()

  private def solver(): Iterator[Solution] = {

    for (
      nations <- Nation.permute
        // 10. The Norwegian lives in the first house.
        if nations.indexOf(Nation.Norwegian) == 0;

      colors <- Color.permute
        // 2. The Englishman lives in the red house.
        if nations.indexOf(Nation.English) == colors.indexOf(Color.Red)

        // 15. The Norwegian lives next to the blue house.
        if (nations.indexOf(Nation.Norwegian) - colors.indexOf(Color.Blue)).abs == 1

        // 6. The green house is immediately to the right of the ivory house.
        if colors.indexOf(Color.Ivory) + 1 == colors.indexOf(Color.Green);

      hobbies <- Hobby.permute
        // 8. The person in the yellow house is a painter.
        if hobbies.indexOf(Hobby.Painting) == colors.indexOf(Color.Yellow)

        // 14. The Japanese person plays chess.
        if hobbies.indexOf(Hobby.Chess) == nations.indexOf(Nation.Japanese);

      animals <- Animal.permute
        // 11. The person who enjoys reading lives in the house next to the person with the fox.
        if (hobbies.indexOf(Hobby.Reading) - animals.indexOf(Animal.Fox)).abs == 1

        // 12. The painter's house is next to the house with the horse.
        if (hobbies.indexOf(Hobby.Painting) - animals.indexOf(Animal.Horse)).abs == 1

        // 3. The Spaniard owns the dog.
        if nations.indexOf(Nation.Spanish) == animals.indexOf(Animal.Dog)

        // 7. The snail owner likes to go dancing.
        if hobbies.indexOf(Hobby.Dancing) == animals.indexOf(Animal.Snail);

      drinks <- Drink.permute

        // 9. The person in the middle house drinks milk.
        if drinks.indexOf(Drink.Milk) == 2

        // 5. The Ukrainian drinks tea.
        if nations.indexOf(Nation.Ukrainian) == drinks.indexOf(Drink.Tea)

        // 4. The person in the green house drinks coffee.
        if colors.indexOf(Color.Green) == drinks.indexOf(Drink.Coffee)

        // 13. The person who plays football drinks orange juice.
        if hobbies.indexOf(Hobby.Football) == drinks.indexOf(Drink.OrangeJuice)

    ) yield {
      Solution(
        waterDrinker = nationToResident(nations(drinks.indexOf(Drink.Water)).toString),
        zebraOwner = nationToResident(nations(animals.indexOf(Animal.Zebra)).toString)
      )
    }
  }

  def nationToResident(nation: String): Resident =
    Nation.withName(nation) match {
      case Nation.English => Englishman
      case Nation.Spanish => Spaniard
      case Nation.Ukrainian => Ukrainian
      case Nation.Norwegian => Norwegian
      case Nation.Japanese => Japanese
    }
}


trait PermutableEnumeration extends Enumeration {
  def permute: Iterator[List[_]] = this.values.toList.permutations
}

object Nation extends PermutableEnumeration {
  type Nation = Value
  val English, Spanish, Ukrainian, Norwegian, Japanese = Value
}

object Color extends PermutableEnumeration {
  type Color = Value
  val Red, Green, Ivory, Yellow, Blue = Value
}

object Hobby extends PermutableEnumeration {
  type Hobby = Value
  val Chess, Football, Dancing, Painting, Reading = Value
}

object Animal extends PermutableEnumeration {
  type Animal = Value
  val Dog, Snail, Fox, Horse, Zebra = Value
}

object Drink extends PermutableEnumeration {
  type Drink = Value
  val Coffee, Milk, OrangeJuice, Tea, Water = Value
}
