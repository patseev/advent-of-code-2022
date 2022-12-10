import java.nio.charset.Charset

import MyApp.getArgs
import zio.ZIO
import zio.stream.ZPipeline
import zio.stream.ZStream

object Solution {
  def print(number: Int, solutions: List[String]) =
    for {
      _ <- accentPrint("=" * 21)
      _ <- accentPrint(s"🎄 Task $number Solution 🎄")
      _ <- accentPrint("=" * 21)
      _ <- ZIO.foreach(solutions)(solutionPrint)
    } yield ()

  private def accentPrint(string: String) =
    zio.Console.printLine(s"${Console.MAGENTA}$string${Console.RESET}")

  private def solutionPrint(string: String) =
    zio.Console.printLine(s"${Console.GREEN}$string${Console.RESET}")

  def openInputFile(filename: String) =
    ZStream.fromFileName(filename).via(ZPipeline.utf8Decode).via(ZPipeline.splitLines)

  def openInputFileAsChars(filename: String) =
    ZStream.fromFileName(filename).via(ZPipeline.decodeCharsWith(Charset.defaultCharset()))
}
