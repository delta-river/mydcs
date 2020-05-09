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
    val a_new = a.map(keep_index)
    val sigma_new = sigma.(keep_index)
    new Denotation(a_new, sigma_new)
  }

  private def simple_join(that: Denotation) : Denotation = {
    val a_new = this.a ++ that.a
    val sigma_new = this.sigma ++ that.sigma
    val arity_new = this.arity + that.arity
    new Denotation(arity_new, a_new, sigma_new)
  }

  def join_proj(i: Int, j: Int, that: Denotation) : Denotation = {
    //doesnt check if i, j is valid index
    val a_new = this.a.flatMap{this_l: List[TupleValue] => that.a.flatMap{that_l: List[TupleValue] => if (this_l(i) == that_l(j)) Some(this_l ++ that_l) else None}}
    val sigma_new = this.sigma ++ that.sigma
    (new Denotation(a_new, sigma_n)).remove
  }
}

object Denotation{

  //calculate denotation of tree
  def from_tree(t: Tree, world: World) : Denotation = {
    //base case
    def leaf(pred: Predicate) : Denotation = new Denotation(world.values(pred).v, Store.empty)

    //step case
    def step(children: List[(Relation, Tree)], acc: Denotation) : Denotation = {
      children match {
        case (e, c)::rem_chil =>{
          e match {
            //for now only join relation
            case Join(i, j) => {
              val child_denote = from_tree(c, world)
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
