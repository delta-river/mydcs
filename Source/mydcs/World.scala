package mydcs

class World(_database: DataBase, _predMap: Map[Predicate, List[TalbeInfo]]) {
  //predicate to tableinfo
  private val predMap : Map[Predicate, List[TableInfo]] = _predMap
  private val database : DataBase = _database
  def values(p: Predicate) = world_map(p)
}

object World{
}
