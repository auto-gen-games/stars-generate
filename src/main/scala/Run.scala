object Run extends App {
  val engine = Engine (stars = 3, numberOfPuzzles = 1, puzzleFilePrefix = "stars3-1-", cellPixels = 10)
  import engine._

  val puzzles = generate.generatePuzzles (numberOfPuzzles)
  puzzles.zipWithIndex.map (p => output.drawPuzzle (p._1._1, puzzleFilePrefix + p._2 + "-puzzle.png"))
  puzzles.zipWithIndex.map (p => output.drawSolution (p._1._1, p._1._2, puzzleFilePrefix + p._2 + "-solution.png"))
  println ("Done")
}
