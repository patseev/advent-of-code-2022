import zio.stream.ZStream

object Task1 {
  private case class Elf(
      number: Int,
      caloriesCount: Int,
    )

  private case class State(
      currentCalories: Int,
      elves: Vector[Elf],
    )

  private object State {
    lazy val initial = State(currentCalories = 0, elves = Vector.empty)
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      top1 <- calculateCalories(input)(topN = 1).map(_.getOrElse(0))
      top3 <- calculateCalories(input)(topN = 3).map(_.getOrElse(0))
      _ <- Solution.print(1, List(s"Top 1: $top1", s"Top 3: $top3"))
    } yield ()

  private def calculateCalories(input: ZStream[Any, Throwable, String])(topN: Int) =
    input
      .mapAccum(State.initial) {
        case (state, nextLine) =>
          val next = nextLine.toIntOption match {
            /* In case if was possible to parse as int, means that current line is calories count */
            case Some(calories) =>
              state.copy(
                currentCalories = state.currentCalories + calories
              )
            /* Otherwise, it's a gap between elves */
            case None =>
              state.copy(
                currentCalories = 0,
                elves = state.elves.appended(Elf(state.elves.size + 1, state.currentCalories)),
              )
          }

          next -> next.elves.sortBy(-_.caloriesCount).take(topN).map(_.caloriesCount).sum
      }
      .runLast
}
