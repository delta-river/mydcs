package mydcs

class World(_database: DataBase) {

  //custom-predicate to tableinfo
  private val predMap : Map[CustomPredicate, List[TableInfo]] = _predMap

  //main database
  private val database : DataBase = _database

  def values(p: Predicate) : Set[TupleValue] = {
    p match{
      case NamePredicate(name, tag) => Set(SymbolicValue(name, tag))
      //ignore invalid tableinfo, which is not contained in the database 
      case CustomPredicate(name, info) => info.flatMap(database.lookup(_)).foldLeft(Set[TupleValue]()){(s1, s2) => s1 | s2}
    }
  }

}

object World{
}
