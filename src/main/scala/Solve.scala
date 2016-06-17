import Types._

class Solve (engine: Engine) {
  import engine._

  def validChange (puzzle: Puzzle, newPartial: Partial, row: Int, column: Int): Boolean =
    regionPotentialStars (puzzle, newPartial, puzzle.regionAt (row, column)) >= stars &&
      regionStars (puzzle, newPartial, puzzle.regionAt (row, column)) <= stars &&
      lengthPotentialStars (newPartial, row, isRow = true) >= stars &&
      lengthStars (newPartial, row, isRow = true) <= stars &&
      lengthPotentialStars (newPartial, column, isRow = false) >= stars &&
      lengthStars (newPartial, column, isRow = false) <= stars

  def hypothesise (puzzle: Puzzle, state: Partial, row: Int, column: Int, assumed: CellHypothesis): Option[Partial] =
    hypothesise (puzzle, state, Vector ((row, column, assumed)))

  def hypothesise (puzzle: Puzzle, state: Partial, hypotheses: Vector[Hypothesis]): Option[Partial] = {
    if (hypotheses.isEmpty) Some (state) else {
      val row = hypotheses.head._1
      val column = hypotheses.head._2
      val assumed = hypotheses.head._3
      val newPartial = state.updated (row, state (row).updated (column, assumed))

      state (row)(column) match {
        case UnknownCell if validChange (puzzle, newPartial, row, column) =>
          assumed match {
            case StarCell =>
              val toEmpty = neighbours ((row, column)).filter (c => newPartial (c._1)(c._2) != EmptyCell) ++
                ((if (regionStars (puzzle, newPartial, puzzle.regionAt (row, column)) == stars)
                  puzzle.regionCoordinates (puzzle.regionAt (row, column)) else Nil) ++
                (if (lengthStars (newPartial, row, isRow = true) == stars)
                  puzzle.rowCoordinates (row) else Nil) ++
                (if (lengthStars (newPartial, column, isRow = false) == stars)
                  puzzle.columnCoordinates (column) else Nil)).
                filter (c => newPartial (c._1)(c._2) == UnknownCell)
              hypothesise (puzzle, newPartial, (toEmpty.map (c => (c._1, c._2, EmptyCell)) ++ hypotheses.tail).distinct)
            case EmptyCell =>
              val toFill =
                ((if (regionPotentialStars (puzzle, newPartial, puzzle.regionAt (row, column)) == stars)
                  puzzle.regionCoordinates (puzzle.regionAt (row, column)) else Nil) ++
                  (if (lengthPotentialStars (newPartial, row, isRow = true) == stars)
                    puzzle.rowCoordinates (row) else Nil) ++
                  (if (lengthPotentialStars (newPartial, column, isRow = false) == stars)
                    puzzle.columnCoordinates (column) else Nil)).
                  filter (c => newPartial (c._1)(c._2) == UnknownCell)
              hypothesise (puzzle, newPartial, (toFill.map (c => (c._1, c._2, StarCell)).toVector ++ hypotheses.tail).distinct)
          }
        case _ =>
          if (state (row)(column) == assumed) Some (state) else None
      }
    }
  }

  def regionStars (puzzle: Puzzle, partial: Partial, region: Int): Int =
    puzzle.regionCoordinates (region).count (c => partial (c._1)(c._2) == StarCell)

  def regionPotentialStars (puzzle: Puzzle, partial: Partial, region: Int): Int =
    puzzle.regionCoordinates (region).count (c => partial (c._1)(c._2) != EmptyCell)

  def lengthStars (partial: Partial, index: Int, isRow: Boolean): Int =
    axis.count (other => if (isRow) partial (index)(other) == StarCell else partial (other)(index) == StarCell)

  def lengthPotentialStars (partial: Partial, index: Int, isRow: Boolean): Int =
    axis.count (other => if (isRow) partial (index)(other) != EmptyCell else partial (other)(index) != EmptyCell)

  def orderCellsByCriticality (puzzle: Puzzle): Vector[(Int, Int)] =
    puzzle.regions.sortBy (puzzle.regionCoordinates (_).size).flatMap (puzzle.regionCoordinates)

  def solutions (puzzle: Puzzle, stopAt2: Boolean): Vector[Partial] = {
    val orderedCoords = orderCellsByCriticality (puzzle)

    def solutionsAssuming (partials: Vector[Partial], found: Vector[Partial]): Vector[Partial] = {
      if (partials.isEmpty) found else
      orderedCoords.find (c => partials.head (c._1)(c._2) == UnknownCell) match {
        case None =>
          if (stopAt2 && found.nonEmpty)
            partials.head +: found
          else
            solutionsAssuming (partials.tail, partials.head +: found)
        case Some ((row, column)) =>
          solutionsAssuming (hypothesise (puzzle, partials.head, row, column, StarCell).toVector ++ partials.tail ++
            hypothesise (puzzle, partials.head, row, column, EmptyCell), found)
      }
    }

    solutionsAssuming (Vector (Vector.fill[CellSolutionState] (size, size)(UnknownCell)), Vector[Partial] ())
  }
}
