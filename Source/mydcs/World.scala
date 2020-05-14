package mydcs

class World(_database: DataBase) {

  //main database
  private val database : DataBase = _database

  def values(p: Predicate) : Set[List[TupleValue]] = {
    p match{
      case NamePredicate(name, tag) => Set(List(TupleValue(List(SymbolicValue(name, tag)))))
      //ignore invalid tableinfo, which is not contained in the database 
      case CustomPredicate(_, info, _) => info.flatMap(database.lookup(_)).foldLeft(Set[List[TupleValue]]()){(s1, s2) => s1 | s2.map{case v => List(v)}}
    }
  }

}
