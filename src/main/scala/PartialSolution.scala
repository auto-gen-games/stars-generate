class PartialSolution (puzzle: Puzzle, states: Vector[Vector[CellSolutionState]], toTry: Vector[(Int, Int)],
                       parent: Option[PartialSolution], val assumptions: Int) {
  def assume (row: Int, column: Int, assumed: CellHypothesis): PartialSolution =
    new PartialSolution (puzzle, states.updated (row, states (row).updated (column, assumed)),
      toTry.filter (_ != (row, column)), Some (this), assumptions + 1)

  def conclude (row: Int, column: Int, assumed: CellHypothesis): PartialSolution =
    new PartialSolution (puzzle, states.updated (row, states (row).updated (column, assumed)),
      toTry.filter (_ != (row, column)), Some (this), assumptions)

  def state (row: Int, column: Int): CellSolutionState =
    states (row)(column)

  def regionStars (puzzle: Puzzle, region: Int): Int =
    puzzle.regionCoordinates (region).count (c => states (c._1)(c._2) == StarCell)

  def regionPotentialStars (puzzle: Puzzle, region: Int): Int =
    puzzle.regionCoordinates (region).count (c => states (c._1)(c._2) != EmptyCell)

  def rowStars (row: Int): Int =
    puzzle.rowCoordinates (row).count (c => states (c._1)(c._2) == StarCell)

  def columnStars (column: Int): Int =
    puzzle.columnCoordinates (column).count (c => states (c._1)(c._2) == StarCell)

  def rowPotentialStars (row: Int): Int =
    puzzle.rowCoordinates (row).count (c => states (c._1)(c._2) != EmptyCell)

  def columnPotentialStars (column: Int): Int =
    puzzle.columnCoordinates (column).count (c => states (c._1)(c._2) != EmptyCell)

  def orderCellsByCriticality (puzzle: Puzzle): Vector[(Int, Int)] =
    puzzle.regions.sortBy (puzzle.regionCoordinates (_).size).flatMap (puzzle.regionCoordinates)
}
