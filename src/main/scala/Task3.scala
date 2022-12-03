import zio.ZIO
import zio.stream.ZStream

object Task3 {
  case class ElfGroup(
      rucksack1: Rucksack,
      rucksack2: Rucksack,
      rucksack3: Rucksack,
    ) {
    def badgeItem =
      ZIO.getOrFail {
        (rucksack1.items intersect
          rucksack2.items intersect
          rucksack3.items).headOption
      }

    def badgeItemPriority =
      badgeItem.map { item =>
        if (item < 'a') item.toInt - 38
        else item.toInt - 96
      }
  }

  case class Rucksack(
      compartment1: Set[Char],
      compartment2: Set[Char],
    ) {
    def offenderItem =
      ZIO.getOrFail(compartment1.intersect(compartment2).headOption)

    def offenderItemPriority =
      offenderItem.map { item =>
        if (item < 'a') item.toInt - 38
        else item.toInt - 96
      }

    def items = compartment1.union(compartment2)
  }

  object Rucksack {
    def parse(string: String) = {
      val (compartment1, compartment2) = string.splitAt(string.length / 2)
      Rucksack(compartment1.toSet, compartment2.toSet)
    }
  }

  object part1 {
    def processLines(input: ZStream[Any, Throwable, String]) =
      input
        .map(Rucksack.parse)
        .mapZIO(_.offenderItemPriority)
        .runSum
  }

  object part2 {
    def processLines(input: ZStream[Any, Throwable, String]) =
      input
        .map(Rucksack.parse)
        .grouped(3)
        .mapZIO(_.toList match {
          case r1 :: r2 :: r3 :: Nil => ZIO.from(ElfGroup(r1, r2, r3))
          case _ => ZIO.fail(new RuntimeException("Could not create a group of elfs"))
        })
        .mapZIO(_.badgeItemPriority)
        .runSum
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      result1 <- part1.processLines(input)
      result2 <- part2.processLines(input)
      _ <- Solution.print(2, List(s"Final score 1: $result1", s"Final score 2: $result2"))

    } yield ()
}
