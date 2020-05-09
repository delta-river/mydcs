package mydcs

import Types._

class Predicate(val name: String)

object Predicate{
  //empty predicate
  def empty : Predicate = new Predicate("")
  def fromString(name: String): Predicate = new Predicate(name)
}
