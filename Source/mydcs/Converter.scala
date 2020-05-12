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
      val children : List[Option[(Relation, DCSTree)]] = child_trees.flatMap{ child_op: Option[DCSTree] =>
        child_op.map{ child: DCSTree =>
          val child_pred = child.root
          //inserting join realtions
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
              val chosen : Relation = joinable.find{case (i, j) => i < j} match {
                case Some(ret) => Join(ret._1, ret._2)
                case None => {
                  val one = joinable.head
                  Join(one._1, one._2)
                }
              }
              Some((chosen, child))
            }
          }
        }
      }
      if (children.contains(None)) None else {
        val lovely_children = children.flatMap{case c => c}
        Some(new DCSTree(this_pred, lovely_children))
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
    val lextrigger = new LexicalTrigger(path)
    new Converter(predtable, lextrigger)
  }
}
