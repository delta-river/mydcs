package mydcs

sealed trait Relation
case class Join(val i: Int, val j: Int) extends Relation
