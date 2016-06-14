case class RegionSet (regions: Vector[Region]) {
  def allocation (position: (Int, Int)): Option[Region] =
    regions.find (_.positions.contains (position))

  def grow (region: Region, newPosition: (Int, Int)): RegionSet =
    RegionSet (region.grow (newPosition) +: regions.filter (_.origin != region.origin))

  def merge (regionA: Region, regionB: Region): RegionSet =
    RegionSet (regionA.include (regionB) +: regions.filter (r => r.origin != regionA.origin && r.origin != regionB.origin))

  def superceding (oldRegion: Region): Region =
    allocation (oldRegion.origin).get
}
