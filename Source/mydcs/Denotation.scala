package mydcs

class Store(val r: Option[Relation], val b: Option[Denotation], val c: Option[Denotation]){
  def isEmpty : Boolean = r.isEmpty && b.isEmpty && c.isEmpty
}

object Store{
  def empty : Store = new Store(None, None, None)
}

//assuming the length of the A and Sigma is same
class Denotation(val a: Set[List[TupleValue]], val sigma: List[Store]){

  //remove non initial columns with empty stores
  def remove = {
    val keep_index = this.sigma.zipWithIndex.flatMap{case (s, i) => if (s.isEmpty && i > 0) None else Some(i)}
    val a_new = a.map{l : List[TupleValue] => keep_index.map(l)}
    val sigma_new = keep_index.map(sigma)
    new Denotation(a_new, sigma_new)
  }

  def join_proj(i: Int, j: Int, that: Denotation) : Denotation = {
    //doesnt check if i, j is valid index
    //どっちもemptyの場合は結局結果はemptyなので、これでOkだと思われ。
    val a_new = this.a.flatMap{this_l: List[TupleValue] => that.a.flatMap{that_l: List[TupleValue] => if (this_l(0).v(i) == that_l(0).v(j)) Some(this_l ++ that_l) else None}}
    val sigma_new = this.sigma ++ that.sigma

    (new Denotation(a_new, sigma_new)).remove
  }

  def answer : String = {
    a.toSeq match{
      case Seq() => "No"
      case Seq(Nil) => "Yes"
      case l => l.flatMap{v: List[TupleValue] => v.map(_.output)}.mkString("{", ", ", "}")
    }
  }

}

object Denotation{

  //calculate denotation of tree
  def calculate(t: DCSTree, world: World) : Denotation = {
    //base case
    //for a leaf, only one column
    def leaf(pred: Predicate) : Denotation = new Denotation(world.values(pred), List(Store.empty))

    //step case
    def step(children: List[(Relation, DCSTree)], acc: Denotation) : Denotation = {
      children match {
        case (e, c)::rem_chil =>{
          e match {
            //for now only join relation
            //others to be written
            case Join(i, j) => {
              val child_denote = calculate(c, world)
              val denote_new = acc.join_proj(i, j, child_denote)
              step(rem_chil, denote_new)
            }
          }
        }
        case Nil => acc
      }
    }

    val root_deno = leaf(t.root)
    step(t.children, root_deno)
  }

}
