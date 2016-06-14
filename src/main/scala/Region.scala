case class Region (origin: (Int, Int), positions: Vector[(Int, Int)], map: StarMap) {
  def this (origin: (Int, Int), map: StarMap) = this (origin, Vector (origin), map)

  val adjacentTo: Vector[(Int, Int)] =
    positions.flatMap (p => Vector ((p._1 - 1, p._2), (p._1 + 1, p._2), (p._1, p._2 - 1), (p._1, p._2 + 1))).
      filter (p => p._1 >= 0 && p._1 < map.size && p._2 >= 0 && p._2 < map.size && !positions.contains (p))

  val numberOfStars: Int =
    positions.intersect (map.starPositions).size

  def grow (newPosition: (Int, Int)): Region =
    Region (origin, newPosition +: positions, map)

  def include (other: Region): Region =
    Region (origin, positions ++ other.positions, map)

  def uniqueID: Int =
    origin._1 + (origin._2 * map.size)
}
