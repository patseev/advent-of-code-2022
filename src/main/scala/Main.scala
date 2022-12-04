import zio._

object MyApp extends ZIOAppDefault {
  def run =
    for {
      _ <- Task1.solve(Solution.openInputFile("inputs/input1.txt"))
      _ <- Task2.solve(Solution.openInputFile("inputs/input2.txt"))
      _ <- Task3.solve(Solution.openInputFile("inputs/input3.txt"))
      _ <- Task4.solve(Solution.openInputFile("inputs/input4.txt"))
    } yield ()
}
