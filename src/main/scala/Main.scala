import zio._

object MyApp extends ZIOAppDefault {
  def run =
    for {
      _ <- Task1.solve(Solution.openInputFile("input1.txt"))
      _ <- Task2.solve(Solution.openInputFile("input2.txt"))
    } yield ()
}
