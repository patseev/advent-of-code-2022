import zio._

object MyApp extends ZIOAppDefault {
  def run =
    for {
      fileStream <- Solution.openInputFile
      _ <- Task1.solve(fileStream)
    } yield ()
}
