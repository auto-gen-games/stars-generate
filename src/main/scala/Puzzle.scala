class Puzzle private (engine: Engine, cells: Vector[Vector[Int]]) {
  def this (engine: Engine, regions: RegionSet) =
    this (engine, engine.axis.map (r => engine.axis.map (c => regions.allocation ((r, c)).get.uniqueID)))

  import engine._

  val regions: Vector[Int] =
    cells.flatten.distinct

  val regionCoordinates: Map[Int, Vector[(Int, Int)]] =
    regions.map (region => (region, coordinates.filter (c => cells (c._1)(c._2) == region))).toMap

  val rowCoordinates: Map[Int, Vector[(Int, Int)]] =
    axis.map (r => (r, axis.map (c => (r, c)))).toMap

  val columnCoordinates: Map[Int, Vector[(Int, Int)]] =
    axis.map (c => (c, axis.map (r => (r, c)))).toMap

  def regionAt (row: Int, column: Int): Int =
    cells (row)(column)
}
