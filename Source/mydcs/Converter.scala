package mydcs

//convert input tree to DCS tree
//InputTree -> MiddleTree -> DCSTree

class Converter(_predtable: PredicateTable, _lextrigger: LexicalTrigger){

  private val predtable = _predtable
  private val lextrigger = _lextrigger

  private def word2pred(s: String) : Option[Predicate] = lextrigger.lookup(s).flatMap{ trig: String => predtable.lookup(trig)}

  //ignore words that does not have corresponding predicates
  //for now only use lemma
  private def input2mediate(input: InputTree) : List[MediateTree] = {
    val root_lemma = input.root._2
    word2pred(root_lemma) match{
      case Some(pred) => {
        val children = input.children.flatMap(input2mediate)
        List(new MediateTree(pred, children))
      }
      //has no corresponding predicate
      case _ => {
        //just returns its children as list
        input.children.flatMap(input2mediate)
      }
    }
  }

  private def mediate2dcs(mediate: MediateTree) : Option[DCSTree] = {
    val this_pred = mediate.root
    val child_trees : List[Option[DCSTree]] = mediate.children.map(mediate2dcs)
    // if a subtree is not feasible then whole the tree is unfeasible
    if (child_trees.contains(None)) None else {
      //inserting realtions
      val children : List[Option[(Int, (Relation, DCSTree))]] = child_trees.flatMap{ child_op: Option[DCSTree] =>
        child_op.map{ child: DCSTree =>
          val child_pred = child.root
          //try inserting join realtions
          val joinable = this_pred.joinable(child_pred)
          joinable match{
            //insert trace predicate
            case Nil => {
              //just igonre for now
              return None
              /*
              val trace_preds : List[Predicate] = predtable.realted_pred(this_pred, child_pred)
                //to be written
              */
            }
            case _ => {
              //huristics
              //assuming that root will take the domain part of values
              val chosen : (Int, Join) = joinable.find{case (k, (i, j)) => i < j} match {
                case Some(ret) => (ret._1, Join(ret._2._1, ret._2._2))
                case None => {
                  val (k, one) = joinable.head
                  (k, Join(one._1, one._2))
                }
              }
              Some((chosen._1, (chosen._2, child)))
            }
          }
        }
      }
      if (children.contains(None)) None else {
        val pretty_children = children.flatMap{case c => c}
        val lovely_children = pretty_children.map(_._2)
        this_pred match {
          //for namepred, if it has feasible children then it is ok(already checked to be feasible)
          case _:NamePredicate => Some(new DCSTree(this_pred, lovely_children))
          case prepre:CustomPredicate => {
            val chosen_indices = pretty_children.map(_._1)
            chosen_indices match{
              case Nil => Some(new DCSTree(prepre, lovely_children))
              case _ => {
                val head = chosen_indices.head
                //huristics allow predicate to have only one table information
                //i.e. does not allow amguity over tables
                //if the correct relation is chosen its ok, though might be too strong....
                //to be worked on
                if (chosen_indices.forall{i:Int => i == head}) Some(new DCSTree(prepre.proj_away(head), lovely_children)) else None
              }
            }
          }
        }
      }
    }
  }

  def convert(input: InputTree) : Option[DCSTree] = {
    val mediate : List[MediateTree] = input2mediate(input)
    mediate match{
      case Nil => None
      case _ => mediate2dcs(mediate.head)
    }
  }

}

object Converter{
  def from(path: String, database: DataBase) : Converter = {
    val predtable = new PredicateTable(path, database)
    val lextrigger = new LexicalTrigger(path, predtable)
    new Converter(predtable, lextrigger)
  }
}
