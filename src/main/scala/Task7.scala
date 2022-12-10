import zio.stream.ZStream

object Task7 {
  object common {
    type DirectoryName = String
    type PathDirectoryName = String

    type Path = List[String]
    case class Directory(path: Path)
    case class File(path: Path, size: Long)

    case class State(
        directories: Set[Directory],
        files: Set[File],
        currentPath: Path,
      ) {
      def directorySizes: Map[Path, Long] =
        directories.map { directory =>
          val size = files.filter(_.path.startsWith(directory.path)).map(_.size).sum
          directory.path -> size
        }.toMap
    }

    object State {
      def initial = State(Set.empty, Set.empty, currentPath = List.empty)
    }

    def parse(input: ZStream[Any, Throwable, String]) =
      input
        .runFold(State.initial) {
          case (state, next) =>
            next match {
              case "$ cd .." =>
                state.copy(currentPath = state.currentPath.dropRight(1))
              case "$ cd" =>
                state.copy(currentPath = List.empty)

              case s"$$ cd $path" =>
                state.copy(currentPath = state.currentPath :+ path)

              case s"$$ ls" =>
                state

              case s"dir $name" =>
                state.copy(
                  directories = state.directories + Directory(state.currentPath :+ name)
                )

              case s"$fileSize $_" =>
                state.copy(
                  files = state.files + File(state.currentPath, fileSize.toLong)
                )
            }
        }
  }

  object part1 {
    def solve(input: ZStream[Any, Throwable, String]) =
      for {
        state <- common.parse(input)
        result = state.directorySizes.values.filter(_ <= 100_000).sum
      } yield result
  }

  object part2 {
    def solve(input: ZStream[Any, Throwable, String]) =
      for {
        state <- common.parse(input)
        totalSpace = 70_000_000
        requiredForUpdate = 3_000_0000
        takenSpace = state.files.map(_.size).sum
        freeSpace = totalSpace - takenSpace
        spaceToFree = requiredForUpdate - freeSpace
        result = state.directorySizes.values.toList.sorted.dropWhile(_ < spaceToFree).take(1).head
      } yield result
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      result1 <- part1.solve(input.drop(1))
      result2 <- part2.solve(input.drop(1))
      _ <- Solution.print(7, List(s"Final score 1: $result1", s"Final score 2: $result2"))
    } yield ()
}
