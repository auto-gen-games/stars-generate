import Types._

class Solve (engine: Engine) {
  import engine._

  def validChange (puzzle: Puzzle, newPartial: Partial, row: Int, column: Int): Boolean =
    regionPotentialStars (puzzle, newPartial, puzzle (row)(column)) >= stars &&
      regionStars (puzzle, newPartial, puzzle (row)(column)) <= stars &&
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
                ((if (regionStars (puzzle, newPartial, puzzle (row)(column)) == stars)
                  regionCoordinates (puzzle, puzzle (row)(column)) else Nil) ++
                (if (lengthStars (newPartial, row, isRow = true) == stars)
                  lengthCoordinates (row, isRow = true) else Nil) ++
                (if (lengthStars (newPartial, column, isRow = false) == stars)
                  lengthCoordinates (column, isRow = false) else Nil)).
                filter (c => newPartial (c._1)(c._2) == UnknownCell)
              hypothesise (puzzle, newPartial, (toEmpty.map (c => (c._1, c._2, EmptyCell)) ++ hypotheses.tail).distinct)
            case EmptyCell =>
              val toFill =
                ((if (regionPotentialStars (puzzle, newPartial, puzzle (row)(column)) == stars)
                  regionCoordinates (puzzle, puzzle (row)(column)) else Nil) ++
                  (if (lengthPotentialStars (newPartial, row, isRow = true) == stars)
                    lengthCoordinates (row, isRow = true) else Nil) ++
                  (if (lengthPotentialStars (newPartial, column, isRow = false) == stars)
                    lengthCoordinates (column, isRow = false) else Nil)).
                  filter (c => newPartial (c._1)(c._2) == UnknownCell)
              hypothesise (puzzle, newPartial, (toFill.map (c => (c._1, c._2, StarCell)).toVector ++ hypotheses.tail).distinct)
          }
        case _ =>
          //println ("Covering " + state (row)(column))
          if (state (row)(column) == assumed) Some (state) else None
      }
    }
  }

  def validCoordinate (coordinate: (Int, Int)): Boolean =
    coordinate._1 >= 0 && coordinate._2 >= 0 && coordinate._1 < size && coordinate._2 < size

  def translate (coordinate: (Int, Int), difference: (Int, Int)): Option[(Int, Int)] = {
    val translated = (coordinate._1 + difference._1, coordinate._2 + difference._2)
    if (validCoordinate (translated)) Some (translated) else None
  }

  def neighbours (coordinate: (Int, Int)): Vector[(Int, Int)] =
    directions.flatMap (d => translate (coordinate, d))

  def starAdjacents (partial: Partial): Vector[(Int, Int)] =
    coordinates.filter (c => neighbours (c).exists (n => partial (n._1)(n._2) == StarCell))

  def allRegions (puzzle: Puzzle): Vector[Int] =
    puzzle.flatten.distinct

  def regionCoordinates (puzzle: Puzzle, region: Int): Vector[(Int, Int)] =
    coordinates.filter (c => puzzle (c._1)(c._2) == region)

  def lengthCoordinates (index: Int, isRow: Boolean): Vector[(Int, Int)] =
    if (isRow) axis.map (c => (index, c)) else axis.map (r => (r, index))

  def regionStars (puzzle: Puzzle, partial: Partial, region: Int): Int =
    regionCoordinates (puzzle, region).count (c => partial (c._1)(c._2) == StarCell)

  def regionPotentialStars (puzzle: Puzzle, partial: Partial, region: Int): Int =
    regionCoordinates (puzzle, region).count (c => partial (c._1)(c._2) != EmptyCell)

  def lengthStars (partial: Partial, index: Int, isRow: Boolean): Int =
    axis.count (other => if (isRow) partial (index)(other) == StarCell else partial (other)(index) == StarCell)

  def lengthPotentialStars (partial: Partial, index: Int, isRow: Boolean): Int =
    axis.count (other => if (isRow) partial (index)(other) != EmptyCell else partial (other)(index) != EmptyCell)

  def orderCellsByCriticality (puzzle: Puzzle): Vector[(Int, Int)] =
    allRegions (puzzle).sortBy (regionCoordinates (puzzle, _).size).flatMap (regionCoordinates (puzzle, _))

  def solutions (puzzle: Puzzle): Vector[Partial] = {
    val orderedCoords = orderCellsByCriticality (puzzle)

    def solutionsAssuming (partials: Vector[Partial], found: Vector[Partial]): Vector[Partial] = {
      if (partials.isEmpty) found else
      orderedCoords.find (c => partials.head (c._1)(c._2) == UnknownCell) match {
        case None =>
          solutionsAssuming (partials.tail, partials.head +: found)
        case Some ((row, column)) =>
          /*println ("A: " + orderedCoords.count (c => partials.head (c._1)(c._2) == UnknownCell))
          val hypothesis1 = hypothesise (puzzle, partials.head, row, column, StarCell)
          hypothesis1.foreach (s => println ("B: " + orderedCoords.count (c => s (c._1)(c._2) == UnknownCell)))
          val hypothesis2 = hypothesise (puzzle, partials.head, row, column, EmptyCell)
          hypothesis2.foreach (s => println ("C: " + orderedCoords.count (c => s (c._1)(c._2) == UnknownCell)))*/
          solutionsAssuming (partials.tail ++ hypothesise (puzzle, partials.head, row, column, StarCell) ++
            hypothesise (puzzle, partials.head, row, column, EmptyCell), found)
      }
    }

    solutionsAssuming (Vector (Vector.fill[CellSolutionState] (size, size)(UnknownCell)), Vector[Partial] ())
  }
}
