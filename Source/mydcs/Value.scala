package mydcs

import Types._

trait Value{
  def output : String
}

trait PrimitiveValue extends Value
case class NumericValue(v:Double, t:Tag) extends PrimitiveValue {
  def output : String = v.toString
}
case class SymbolicValue(v:String, t:Tag) extends PrimitiveValue{
  def output : String = v
}

//only to allow values of the same type to be included
//case class SetValue[+T <: Value](v:Set[T]) extends Value {
//assume it is satisfied
case class SetValue(v:Set[Value]) extends Value {
  def push(s:Value) : SetValue = new SetValue(this.v + s)

  def output : String = v.toList.map(_.output).mkString("{", ", ", "}")
  /*
  def output : String = {
    //with this code, ", " will always be at the begging of the word
    val content : String = v.foldLeft(""){case (l, r) => l + ", " + r.output}
    "{" + {if (content.length > 0) content.drop(2) else content} + "}"
  }
  */
}

//not to use this for simplification
//abstract class NonTupleValue extends PrimitiveValue with SetValue

//only to allow nontuple value to be its component
//case class TupleValue(v:List[NonTupleValue]) extends Value{
case class TupleValue(v:List[Value]) extends Value{
  def project(index: List[Int]) : TupleValue = new TupleValue(index.map(this.v))

  def output : String = if (v.length == 1) v.head.output else {
    v.map(_.output).mkString("(", ", ", ")")
  }

}

object SetValue{
  def empty() : SetValue = {
    val emp : Set[Value] = Set()
    new SetValue(emp)
  }
}

trait AbstractValue
case class AbstractSymb(t: Tag) extends AbstractValue
case class AbstractNum(t: Tag) extends AbstractValue
case class AbstractSet(v: AbstractValue) extends AbstractValue
case class AbstractTup(l: List[AbstractValue]) extends AbstractValue {
  //returns all matching indices, not to choose here
  def matching(that: AbstractTup) : List[(Int, Int)] = {
    this.l.zipWithIndex.flatMap{case (this_ab, i) => that.l.zipWithIndex.flatMap{ case (that_ab, j) => if (this_ab == that_ab) Some((i, j)) else None}}
  }
}
