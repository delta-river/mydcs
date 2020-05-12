package mydcs

import scala.io.Source
import scala.util.matching.Regex

import Types._

trait Predicate{
  val name : String
  //use this information when inserting relation
  val abst : AbstractTup

  def joinable(that: Predicate) : List[(Int, Int)] = abst.matching(that.abst)
}

case class NamePredicate(val name: String, val t: Tag) extends Predicate {
  val abst = AbstractTup(AbstractSymb(t))
}
case class CustomPredicate(val name: String, val info: List[TableInfo], val abst: AbstractTup) extends Predicate{
  def add_info(info_new: TableInfo) : CustomPredicate = new CustomPredicate(name, info + info_new, abst)
  def add_abst(abst_new: Option[AbstractValue]) : CustomPredicate = {
    abst_new match{
      case Some(abs) => new CustomPredicate(name, info, abst + abs)
      case _ => this
    }
  }
}

/* companion object cannot be set for trait
object Predicate{
  //empty predicate
  def empty : Predicate = new Predicate("")
  def fromString(name: String): Predicate = new Predicate(name)
}
*/

object CustomPredicate{
  def named_empty(name: String) : CustomPredicate = new CustomPredicate(name, List[TableInfo](), List[AbstractValue]())
}

//load custom predicate from predicate tag file
//style of predicate tag file *.pred is as below
//pred1_name:table_name:column1\tcolumn2...\n
//...
//if same name is unsed for predicate in different lines, information will be added

class PredicateTable(base_path: String, database: Database){
  
  // no error handling
  private val path = base_path + ".pred"
  private val source = Source.fromFile(path)

  private val custom_predMap : Map[String, Predicate] = {
    val lines = source.getLines
    load(lines, Map[String, Predicate]())
  }

  private val name_predMap : Map[String, Predicate] = {
    database.name_pred.foldLeft(Map[String, Predicate]()){case (l, r) => l + (r.name->r)}
  }

  private val predTable = custom_predMap ++ name_predMap

  def lookup(s: String) : Option[Predicate] = predTable.get(s)

  //enumerate predicates that can be used as trace predicate (only considering CustomPred)
  def related_pred(p1: Predicate, p2: Predicate) : List[Predicate] = {
    custom_predMap.values.filter{ trace: Predicate =>
      val p1_joinable = p1.joinable(trace)
      val p2_joinable = trace.joinable(p2)
      //check if there is joinables pairs that does not share the same column of trace
      p1_joinable.exists{ case (i1, j1) => p2_jonable.exists{case (i2, j2) => j1 != i2}}
    }
  }

  private val custom_exp = """([^:]+):([^:]+):(.+)\n""".r

  private def load(lines: List[String], acc_map: Map[String, Predicate]) : Map[String, Predicate] = {
    lines match{
      case y::yl => {
        y match{
          case custom_exp(name, table, columns, _) =>{
            val column_list = columns.split("\t")
            val info_new = new TableInfo(table, column_list)
            val abst_new : Option[AbstractValue]= database.get_abst(info_new)
            val pred_new = acc_map.getOrElse(name, CustomPredicate.named_empty(name)).add_info(info_new).add_abst(abst_new)
            val map_new = acc_map + (name->pred_new)
          }
          //ignore line of just "\n"
          case "\n" => load(yl, acc_map)
          case _ => {
            println("!!!!!!!!!!!!!!!!!predicate ignored: style miss-match")
            println(y)
            println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            load(yl, acc_map)
          }
        }
      }
      //finish
      case _ => acc_map
    }
  }

}
