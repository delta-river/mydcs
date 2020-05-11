package mydcs

//convert input tree to DCS tree

class Converter(_predtable: PredicateTable, _lextrigger: LexicalTrigger){

  private val predtable = _predtable
  private val lextrigger = _lextirgger

  def word2pred(s: String) : Option[Predicate] = lextrigger.lookup(s).flatMap{ trig: String => predtable.lookup(trig)}
}
