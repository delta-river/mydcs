package mydcs

import QueryParer

class Tree(val root: Predicate, val children : List[(Relation, Tree)]){
  def denote : Denotation
}

object Tree{
  def from_query : Tree
  //to be written
}
