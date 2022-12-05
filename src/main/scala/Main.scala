import zio._

object MyApp extends ZIOAppDefault {
  def run =
    for {
      _ <- Task1.solve(Solution.openInputFile("inputs/input1.txt"))
      _ <- Task2.solve(Solution.openInputFile("inputs/input2.txt"))
      _ <- Task3.solve(Solution.openInputFile("inputs/input3.txt"))
      _ <- Task4.solve(Solution.openInputFile("inputs/input4.txt"))

      _ <- Task5.solve(
        9,
        Solution.openInputFile("inputs/input5_1.txt"),
        Solution.openInputFile("inputs/input5_2.txt"),
      )

//      _ <- Task5.solve(
//        3,
//        Solution.openInputFile("inputs/input5_example1.txt"),
//        Solution.openInputFile("inputs/input5_example2.txt"),
//      )
    } yield ()
}
