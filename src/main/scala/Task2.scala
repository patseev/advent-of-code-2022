import zio.ZIO
import zio.stream.ZStream

object Task2 {
  sealed trait Move extends Product with Serializable {
    import Move._
    import RoundOutcome._

    def score: Int

    def outcome(other: Move): RoundOutcome = (this, other) match {
      case (Rock, Scissors) => RoundOutcome.Win
      case (Paper, Rock) => RoundOutcome.Win
      case (Scissors, Paper) => RoundOutcome.Win
      case (Rock, Paper) => RoundOutcome.Loss
      case (Paper, Scissors) => RoundOutcome.Loss
      case (Scissors, Rock) => RoundOutcome.Loss
      case _ => RoundOutcome.Draw
    }

    def forOutcome(outcome: RoundOutcome): Move = (this, outcome) match {
      case (_, Draw) => this
      case (Paper, Win) => Scissors
      case (Paper, Loss) => Rock
      case (Scissors, Win) => Rock
      case (Scissors, Loss) => Paper
      case (Rock, Win) => Paper
      case (Rock, Loss) => Scissors
    }
  }

  object Move {
    case object Rock extends Move {
      override def score: Int = 1
    }

    case object Paper extends Move {
      def score: Int = 2
    }

    case object Scissors extends Move {
      def score: Int = 3
    }

    def parse(move: String) =
      ZIO.getOrFail(
        move match {
          case "A" | "X" => Some(Rock)
          case "B" | "Y" => Some(Paper)
          case "C" | "Z" => Some(Scissors)
          case _ => None
        }
      )
  }

  sealed trait RoundOutcome extends Product with Serializable {
    def score: Int
  }

  object RoundOutcome {
    case object Win extends RoundOutcome {
      def score = 6
    }

    case object Loss extends RoundOutcome {
      def score = 0
    }

    case object Draw extends RoundOutcome {
      def score = 3
    }

    def parse(outcome: String): zio.Task[RoundOutcome] = ZIO.getOrFail(
      outcome match {
        case "X" => Some(Loss)
        case "Y" => Some(Draw)
        case "Z" => Some(Win)
        case _ => None
      }
    )
  }

  object part1 {
    def oneRound(enemy: Move, you: Move): Int =
      you.outcome(enemy).score + you.score

    def parseLine(line: String) =
      line match {
        case s"$m1 $m2" =>
          for {
            enemy <- Move.parse(m1)
            you <- Move.parse(m2)
          } yield (enemy, you)
      }

    def processLine(line: String) =
      parseLine(line).map { case (enemy, you) => oneRound(enemy, you) }

    def processLines(input: ZStream[Any, Throwable, String]) =
      input.mapZIO(processLine).runSum
  }

  object part2 {
    def oneRound(enemy: Move, you: Move): Int =
      you.outcome(enemy).score + you.score

    def parseLine(line: String) =
      line match {
        case s"$m1 $m2" =>
          for {
            enemy <- Move.parse(m1)
            outcome <- RoundOutcome.parse(m2)
            you = enemy.forOutcome(outcome)
          } yield (enemy, you)
      }

    def processLine(line: String) =
      parseLine(line).map { case (enemy, you) => oneRound(enemy, you) }

    def processLines(input: ZStream[Any, Throwable, String]) =
      input.mapZIO(processLine).runSum
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      result1 <- part1.processLines(input)
      result2 <- part2.processLines(input)
      _ <- Solution.print(2, List(s"Final score 1: $result1", s"Final score 2: $result2"))

    } yield ()
}
