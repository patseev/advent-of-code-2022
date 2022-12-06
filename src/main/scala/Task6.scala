import zio.stream.ZStream

object Task6 {
  case class State(
      lastSeen: Vector[Char],
      marketIndex: Long,
    )

  def findMarker(input: ZStream[Any, Throwable, Char], signalLength: Long) =
    for {
      result <- input
        .zipWithIndex
        .runFoldWhile(State(Vector.empty, 0L))(_.lastSeen.size != signalLength) {
          case (State(lastSeen, _), (char, index)) =>
            if (lastSeen.contains(char))
              State(lastSeen.dropWhile(_ != char).drop(1).appended(char), index)
            else
              State(lastSeen.appended(char), index)
        }
    } yield result.marketIndex + 1

  def solve(input: ZStream[Any, Throwable, Char]) =
    for {
      result1 <- findMarker(input, 4)
      result2 <- findMarker(input, 14)
      _ <- Solution.print(6, List(s"Final score 1: $result1", s"Final score 2: $result2"))
    } yield ()
}
