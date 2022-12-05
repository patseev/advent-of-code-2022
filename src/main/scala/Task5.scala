import scala.collection.mutable
import scala.util.Try

import zio.ZIO
import zio.stream.ZStream

object Task5 {
  sealed trait Crane extends Product with Serializable
  case object `9000` extends Crane
  case object `9001` extends Crane

  case class Command(
      move: Int,
      from: Int,
      to: Int,
    ) {
    override def toString: String = s"[Move $move from $from to $to]"
  }

  object Command {
    def parse(string: String) =
      ZIO.getOrFail(string match {
        case s"move $move from $from to $to" => Some(Command(move.toInt, from.toInt, to.toInt))
        case _ => None
      })
  }

  case class SupplyStacks(
      stacks: Vector[Vector[Char]] = Vector.fill(9)(Vector.empty)
    ) {
    def tops = stacks.flatMap(_.headOption).mkString

    def save(supplyLine: String) = {
      val resultingStacks = stacks.indices.toList.map { stackIndex =>
        // Width of one section is 4
        val charIndex = (stackIndex * 4) + 1
        val stack = Try(supplyLine.charAt(charIndex)).toOption match {
          /* In case if value is on the stack, value at index should be letter */
          case Some(char) if char.isLetter =>
            stacks(stackIndex).appended(char)
          /* Otherwise, we just skip */
          case _ =>
            stacks(stackIndex)
        }

        stack
      }

      SupplyStacks(resultingStacks.toVector)
    }

    def apply(command: Command, crane: Crane) = {
      val movingFrom = stacks(command.from - 1)
      val movingTo = stacks(command.to - 1)

      val (toMove, remained) = movingFrom.splitAt(command.move)

      val moved = crane match {
        /* Crane 9000 puts items in the reverse order that it took them */
        case `9000` => toMove.reverse
        /* Crane 9001 puts items in exact order*/
        case `9001` => toMove
      }

      val finalMovingFrom = remained
      val finalMovingTo = movingTo.prependedAll(moved)

      SupplyStacks(
        stacks.updated(command.from - 1, finalMovingFrom).updated(command.to - 1, finalMovingTo)
      )
    }
  }

  object SupplyStacks {
    def empty(size: Int) = SupplyStacks(Vector.fill(size)(Vector.empty))
  }

  object common {
    def parseStacks(input: ZStream[Any, Throwable, String], stacks: Int) =
      input.runFold(SupplyStacks.empty(stacks)) {
        case (stacks, line) => stacks.save(line)
      }

    def applyCommands(
        stacks: SupplyStacks,
        f: (SupplyStacks, Command) => SupplyStacks,
      )(
        input: ZStream[Any, Throwable, String]
      ) =
      input
        .mapZIO(Command.parse)
        .runFold(stacks)(f)
  }

  object part1 {
    def solve(stacks: SupplyStacks)(input: ZStream[Any, Throwable, String]) =
      common.applyCommands(stacks, _.apply(_, `9000`))(input)
  }

  object part2 {
    def solve(stacks: SupplyStacks)(input: ZStream[Any, Throwable, String]) =
      common.applyCommands(stacks, _.apply(_, `9001`))(input)
  }

  def solve(
      stacks: Int,
      inputStacks: ZStream[Any, Throwable, String],
      inputCommands: ZStream[Any, Throwable, String],
    ) =
    for {
      stacks <- common.parseStacks(inputStacks, stacks)
      result1 <- part1.solve(stacks)(inputCommands)
      result2 <- part2.solve(stacks)(inputCommands)
      _ <- Solution.print(
        5,
        List(s"Final score 1: ${result1.tops}", s"Final score 2: ${result2.tops}"),
      )
    } yield ()
}
