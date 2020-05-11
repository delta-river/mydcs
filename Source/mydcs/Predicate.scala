package mydcs

import scala.io.Source
import scala.util.matching.Regex

import Types._

trait Predicate{
  val name : String
}
case class NamePredicate(val name: String, val t: Tag) extends Predicate
case class CustomPredicate(val name: String, val info: List[TableInfo]) extends Predicate{
  def add_info(info_new: TableInfo) : CustomPredicate = new CustomPredicate(name, info + info_new)
}

object Predicate{
  //empty predicate
  def empty : Predicate = new Predicate("")
  def fromString(name: String): Predicate = new Predicate(name)
}

object CustomPredicate{
  def named_empty(name: String) : CustomPredicate = new CustomPredicate(name, List[TableInfo]())
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

  private val custom_exp = "(.+):(.+):((.+\t)+)\n".r

  private def load(lines: List[String], acc_map: Map[String, Predicate]) : Map[String, Predicate] = {
    lines match{
      case y::yl => {
        y match{
          case custom_exp(name, table, columns, _) =>{
            val column_list = columns.split("\t")
            val info_new = new TableInfo(table, column_list)
            val pred_new = acc_map.getOrElse(name, CustomPredicate.named_empty(name)).add_info(info_new)
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
