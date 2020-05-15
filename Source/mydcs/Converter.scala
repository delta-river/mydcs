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
        //println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!word dropped")
        //println(root_lemma)
        //just returns its children as list
        input.children.flatMap(input2mediate)
      }
    }
  }

  //used only when children do not contain None
  private def feasible_table(root_pred: Predicate, children: List[Option[DCSTree]]) : List[Int] = {
    val joinables: List[List[Int]] = children.flatMap{child_op: Option[DCSTree] => child_op.map{child: DCSTree => root_pred.joinable(child.root).map(_._1).distinct}}
    //find common joiable table index
    joinables match{
      case Nil => Nil
      case y::yl => yl.foldLeft(y){(l, r) => l.intersect(r)}
    }
  }

  private def insert_join(root_pred: Predicate, child: DCSTree) : Option[(Relation, DCSTree)] = {
    val joinable : List[(Int, Int)] = root_pred.joinable(child.root).map(_._2)
    joinable match{
      case Nil => None
      case _ => {
        //huristics
        //assuming that root will take the domain part of values
        val chosen : Join = joinable.find{case (i, j) => i < j} match {
          case Some(ret) => Join(ret._1, ret._2)
          case None => {
            //just take the head
            val one = joinable.head
            Join(one._1, one._2)
          }
        }
        Some(chosen, child)
      }
    }
  }

  private def mediate2dcs(mediate: MediateTree) : Option[DCSTree] = {
    val this_pred = mediate.root
    val child_trees : List[Option[DCSTree]] = mediate.children.map(mediate2dcs)
    // if a subtree is not feasible then whole the tree is unfeasible
    if (child_trees.contains(None)) None 
    //leaf
    else if (child_trees.isEmpty) Some(new DCSTree(this_pred, List[(Relation, DCSTree)]())) else {
      //inserting realtions
      val table_candidate = feasible_table(this_pred, child_trees)
      table_candidate match{
        //insert trace predicate
        //for future work
        case Nil =>/*println("!!!!!!!!!!!!!!!!!!!!!!!!NONE");println(this_pred); */None
        // have to choose feasible one with parent
        // insert join relation
        case _ => {
          val pred_new = this_pred.proj_away(table_candidate)
          val children : List[Option[(Relation, DCSTree)]] = child_trees.flatMap{ c => c}.map(insert_join(pred_new, _))
          if (children.contains(None)) None else Some(new DCSTree(pred_new, children.flatMap{c => c}))
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
