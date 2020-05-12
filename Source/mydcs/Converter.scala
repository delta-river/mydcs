package mydcs

//convert input tree to DCS tree
//InputTree -> MiddleTree -> DCSTree

class Converter(_predtable: PredicateTable, _lextrigger: LexicalTrigger){

  private val predtable = _predtable
  private val lextrigger = _lextirgger

  private def word2pred(s: String) : Option[Predicate] = lextrigger.lookup(s).flatMap{ trig: String => predtable.lookup(trig)}

  //ignore words that does not have corresponding predicates
  //for now only use lemma
  private input2mediate(input: InputTree) : List[MediateTree] = {
    val root_lemma = input.root._2
    word2pred(root_lemma) match{
      case Some(pred) => {
        val children = input.children.flatMap(input2mediate)
        new MediateTree(pred, children)
      }
      //has no corresponding predicate
      case _ => {
        //just returns its children as list
        input.children.faltMap(input2mediate)
      }
    }
  }

  private mediate2dcs(mediate: MediateTree) : Option[DCSTree] = {
    val this_pred = mediate.root
    val child_trees : List[DCSTree] = mediate.children.map(mediate2dcs)
    // if a subtree is not feasible then whole the tree is unfeasible
    if (child_trees.contains(None)) None else {
      //inserting realtions
      val children : List[(Relation, DCSTree)] = child_trees.flatMap{ child: DCSTree =>
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
              case Some(ret) => ret
              case None => joinable.find(_ => true)
            }
            (chosen, child)
          }
        }
      }
      new DCSTree(this_pred, children)
    }
  }

  def convert(input: InputTree) : Option[DCSTree] = {
    val mediate : MediateTree = input2mediate(input)
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
