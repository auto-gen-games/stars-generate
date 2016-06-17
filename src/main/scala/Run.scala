object Run extends App {
  //val engine = Engine (stars = 1, numberOfPuzzles = 1, puzzleFilePrefix = "stars1-1-", cellPixels = 10)
  val engine = Engine (stars = 2, numberOfPuzzles = 10, puzzleFilePrefix = "saved puzzles/stars2-3-", cellPixels = 10)
  //val engine = Engine (stars = 3, numberOfPuzzles = 1, puzzleFilePrefix = "stars3-1-", cellPixels = 10)
  import engine._

  val puzzles = generate.generatePuzzles (numberOfPuzzles)
  puzzles.zipWithIndex.map (p => output.drawPuzzle (p._1._1, puzzleFilePrefix + p._2 + "-puzzle.png"))
  puzzles.zipWithIndex.map (p => output.drawSolution (p._1._1, p._1._2, puzzleFilePrefix + p._2 + "-solution.png"))
  println ("Done")
}

// 14 regions, average size 14, 3 non-adjacent stars each
// 1 star in straight N = N
// 2 stars in straight N = (N - 1)(N - 2)/2
// 3 stars in straight N = (N - 2)(N - 3)(N - 4)/6
// 3 stars in straight 14 = 220
