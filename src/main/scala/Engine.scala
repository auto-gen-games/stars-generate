case class Engine (stars: Int, numberOfPuzzles: Int, puzzleFilePrefix: String, cellPixels: Int) {
  val size = stars * 5 - 1
  val axis = (0 until size).toVector
  val coordinates = axis.flatMap (r => axis.map (c => (r, c)))
  val directions = Vector ((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
  val output = new Output (this)
  val generate = new Generate (this)
  val solve = new Solve (this)
}
