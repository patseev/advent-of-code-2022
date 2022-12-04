import zio.ZIO
import zio.stream.ZStream

object Task4 {
  type SectionID = Int

  case class SectionRange(
      from: SectionID,
      to: SectionID,
    ) {
    def containsRange(other: SectionRange) =
      from <= other.from && to >= other.to

    def overlapsRange(other: SectionRange) =
      from <= other.to && to >= other.from
  }

  object SectionRange {
    def parse(string: String) =
      string.split("-").toList match {
        case first :: second :: Nil =>
          for {
            firstId <- ZIO.getOrFail(first.toIntOption)
            secondId <- ZIO.getOrFail(second.toIntOption)
          } yield SectionRange(firstId, secondId)
        case _ =>
          ZIO.fail(new RuntimeException(s"Assignment couldn't be parsed to SectionRange: $string"))
      }
  }

  case class ElfPair(
      sections1: SectionRange,
      sections2: SectionRange,
    ) {
    def problematicAssignment: Boolean =
      sections1.containsRange(sections2) || sections2.containsRange(sections1)

    def overlappingAssignment: Boolean =
      sections1.overlapsRange(sections2) || sections2.overlapsRange(sections1)
  }

  object ElfPair {
    def parse(string: String) =
      string.split(",").toList match {
        case firstRange :: secondRange :: Nil =>
          for {
            sections1 <- SectionRange.parse(firstRange)
            sections2 <- SectionRange.parse(secondRange)
          } yield ElfPair(sections1, sections2)
        case _ =>
          ZIO.fail(new RuntimeException(s"Assignment couldn't be parsed to ElfPair: $string"))
      }
  }

  object part1 {
    def processLines(input: ZStream[Any, Throwable, String]) =
      input.mapZIO(ElfPair.parse).filter(_.problematicAssignment).runCount
  }

  object part2 {
    def processLines(input: ZStream[Any, Throwable, String]) =
      input.mapZIO(ElfPair.parse).filter(_.overlappingAssignment).runCount
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      result1 <- part1.processLines(input)
      result2 <- part2.processLines(input)
      _ <- Solution.print(4, List(s"Final score 1: $result1", s"Final score 2: $result2"))

    } yield ()
}
