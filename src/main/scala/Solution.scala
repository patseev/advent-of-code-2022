import MyApp.getArgs
import zio.ZIO
import zio.stream.ZPipeline
import zio.stream.ZStream

object Solution {
  def print(number: Int, solutions: List[String]) =
    for {
      _ <- accentPrint("=" * 30)
      _ <- accentPrint(s"Task $number Solution")
      _ <- accentPrint("=" * 30)
      _ <- ZIO.foreach(solutions)(solutionPrint)
    } yield ()

  private def accentPrint(string: String) =
    zio.Console.printLine(s"${Console.MAGENTA}$string${Console.RESET}")

  private def solutionPrint(string: String) =
    zio.Console.printLine(s"${Console.GREEN}$string${Console.RESET}")

  def openInputFile =
    for {
      args <- getArgs
      filename <- ZIO
        .fromOption(args.headOption)
        .orElseFail(new RuntimeException("Filename is required as input"))

      result = ZStream.fromFileName(filename).via(ZPipeline.utf8Decode).via(ZPipeline.splitLines)
    } yield result
}
