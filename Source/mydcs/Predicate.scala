package mydcs

import scala.io.Source
import scala.util.matching.Regex

import Types._

trait Predicate{
  val name : String
  //use this information when inserting relation
  val abst : List[AbstractTup]

  def joinable(that: Predicate) : List[(Int, (Int, Int))] = {
    abst.zipWithIndex.flatMap{case (this_abst, i) => that.abst.flatMap{that_abst: AbstractTup => this_abst.matching(that_abst).map{p: (Int, Int) => (i, p)}}}
  }

  def proj_away(i: List[Int]) : Predicate
}

case class NamePredicate(val name: String, val t: Tag) extends Predicate {
  val abst = List(AbstractTup(List(AbstractSymb(t))))
  //just return itself
  def proj_away(i: List[Int]) = this
}

//assume ith component of info corresponds ith component of abst
case class CustomPredicate(val name: String, val info: List[TableInfo], val abst: List[AbstractTup]) extends Predicate{
  def add_info(info_new: TableInfo) : CustomPredicate = new CustomPredicate(name, info :+ info_new, abst)
  def add_abst(abst_new: Option[AbstractTup]) : CustomPredicate = {
    abst_new match{
      case Some(abs) => new CustomPredicate(name, info, abst :+ abs)
      case _ => this
    }
  }

  def proj_away(i: List[Int]) = new CustomPredicate(name, i.map(info), i.map(abst))
}

/*
//for future work
case class NullPredicate extends Predicate{
  val name : String = ""
}

abstract class FunctionPredicate extends Predicate {
  val operate : List[Value] => Value
}
*/

object CustomPredicate{
  def named_empty(name: String) : CustomPredicate = new CustomPredicate(name, List[TableInfo](), List[AbstractTup]())
}

//load custom predicate from predicate tag file
//style of predicate tag file *.pred is as below
//pred1_name:table_name:column1\tcolumn2...\n
//...
//if same name is unsed for predicate in different lines, information will be added

class PredicateTable(base_path: String, database: DataBase){
  
  // no error handling
  private val path = base_path + ".pred"
  private val source = Source.fromFile(path)

  private val custom_predMap : Map[String, Predicate] = {
    // # for comment out
    val lines = source.getLines.filterNot{s: String => (s.length == 0 || s.startsWith("#"))}
    load(lines.toList, Map[String, CustomPredicate]())
  }

  private val name_predMap : Map[String, Predicate] = {
    database.name_pred.foldLeft(Map[String, Predicate]()){case (l, r) => l + (r.name->r)}
  }

  def namepred_trigger : Map[String, String] = {
    name_predMap.keys.foldLeft(Map[String, String]()){case (l, r) => l + (r->r)}
  }

  //have to be careful about the name collision
  //to be written
  private val predTable = custom_predMap ++ name_predMap

  def lookup(s: String) : Option[Predicate] = predTable.get(s)

  //enumerate predicates that can be used as trace predicate (only considering CustomPred)
  def related_pred(p1: Predicate, p2: Predicate) : List[Predicate] = {
    custom_predMap.values.toList.filter{ trace: Predicate =>
      val p1_joinable = p1.joinable(trace).map(_._2)
      val p2_joinable = trace.joinable(p2).map(_._2)
      //check if there is joinables pairs that does not share the same column of trace
      p1_joinable.exists{ case (i1, j1) => p2_joinable.exists{case (i2, j2) => j1 != i2}}
    }
  }

  //nullpo i dont know why
  //private val custom_exp = """([^:]+):([^:]+):(.+)\n""".r

  private def load(lines: List[String], acc_map: Map[String, CustomPredicate]) : Map[String, CustomPredicate] = {
    val custom_exp = """([^:]+):([^:]+):(.+)""".r
    lines match{
      case y::yl => {
        y match{
          case custom_exp(name, table, columns) =>{
            val column_list = columns.split("\t").toList
            val info_new = new TableInfo(table, column_list)
            val abst_new : Option[AbstractTup]= database.get_abst(info_new)
            val pred_new = acc_map.getOrElse(name, CustomPredicate.named_empty(name)).add_info(info_new).add_abst(abst_new)
            val map_new = acc_map + (name->pred_new)
            load(yl, map_new)
          }
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
