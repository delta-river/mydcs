package mydcs

trait Value

trait PrimitiveValue extends Value
case class NumricValue(v:Double, t:Tag) extends PrimitiveValue
case class SymbolicValue(v:String, t:Tag) extends PrimitiveValue

//only to allow values of the same type to be included
case class SetValue[+T <: Value](v:Set[T]) extends Value {
  def push(s:T) : SetValue = new SetValue(this.v + s)
}

case class NonTupleValue extends PrimitiveValue with SetValue

//only to allow nontuple value to be its component
case class TupleValue(v:List[NonTupleValue]) extends Value{
  def project(index: List[Int]) : TupleValue = new TupleValue(this.v.map(index))
}

object SetValue{
  def empty[T]() : SetValue[T] = {
    val emp : Set[T] = Set()
    new SetValue(emp)
  }
}
