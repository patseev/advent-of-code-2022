import zio.stream.ZStream

object Task8 {
  type Grid[A] = Vector[Vector[A]]
  object Grid {
    lazy val empty =
      Vector.empty[Vector[Int]]
  }

  object common {
    def parse(input: ZStream[Any, Throwable, String]) =
      input
        .runFold(Grid.empty) {
          case (grid, nextLine) =>
            val next = grid :+ nextLine.toVector.map(_.toString.toInt)
            next
        }
  }

  object part1 {
    /* Returns whether elemenent on IxJ position is visible from outside of the grid */
    def checkOutsideVisibility(grid: Grid[Int])(i: Int, j: Int): Boolean =
      /* If element is on first or last row, it's visible */
      if (i == 0 || i == grid.size - 1) true
      /* If element is on first or last column, it's visible */
      else if (j == 0 || j == grid.head.size - 1) true
      /* Else element is visible if no elements to the left, right, top and bottom of it are bigger than in */
      else {
        val element = grid(i)(j)
        val left = grid(i).take(j).forall(_ < element)
        val right = grid(i).drop(j + 1).forall(_ < element)
        val top = grid.take(i).forall(_.apply(j) < element)
        val bottom = grid.drop(i + 1).forall(_.apply(j) < element)
        left || right || top || bottom
      }

    def toOutsideVisibilityGrid(grid: Grid[Int]) =
    // format: off
      grid.indices.map { i =>
        grid(i).indices.map { j =>
          if (checkOutsideVisibility(grid)(i, j)) 1 else 0
        }.toVector
      }.toVector
    // format: on

    def outsideVisibilityResult(visibilityGrid: Grid[Int]) =
      visibilityGrid.map(_.sum).sum

    def solve(grid: Grid[Int]) =
      outsideVisibilityResult(toOutsideVisibilityGrid(grid))
  }

  object part2 {
    type ScenicScore = (Int, Int, Int, Int)

    def calculateScenicScore(grid: Grid[Int])(i: Int, j: Int) = {
      val element = grid(i)(j)

      def calculateViewDistance(elems: Vector[Int]) = {
        val seen = elems.takeWhile(_ < element)
        val isBlocked = seen.size < elems.size
        seen.length + (if (isBlocked) 1 else 0)
      }

      (
        calculateViewDistance(grid.take(i).map(_.apply(j)).reverse),
        calculateViewDistance(grid(i).take(j).reverse),
        calculateViewDistance(grid(i).drop(j + 1)),
        calculateViewDistance(grid.drop(i + 1).map(_.apply(j))),
      )
    }

    def toScenicScoreGrid(grid: Grid[Int]) =
    // format: off
      grid.indices.map { i =>
        grid(i).indices.map { j =>
          calculateScenicScore(grid)(i, j)
        }.toVector
      }.toVector
    // format: on

    def highestScenicScore(grid: Grid[ScenicScore]) =
      grid
        .flatten
        .map {
          case (left, right, top, bottom) =>
            left * right * top * bottom
        }
        .max

    def solve(grid: Grid[Int]) =
      highestScenicScore(toScenicScoreGrid(grid))
  }

  def solve(input: ZStream[Any, Throwable, String]) =
    for {
      grid <- common.parse(input)
      result1 = part1.solve(grid)
      resul2 = part2.solve(grid)
      _ <- Solution.print(8, List(s"Final score 1: $result1", s"Final score 2: $resul2"))
    } yield ()
}
