package mydcs

import Types._

import scala.io.Source
import scala.util.matching.Regex

//prepare database which includes tables
//database style is as below
//table1:\n
//tag1\ttag2...\n
//column1\tcolumn2...\n
//value1\tvalue2...\n
//...

class TableInfo(val table: String, val columns: List[Column])

class DataBase(val database: Map[String, (List[Column], Set[List[PrimitiveValue]])]){

  private def get_index(source: List[Column], index: List[Column]) : List[Int] = {
    index.map{ c: Column => source.indexWhere(_ == c)}
  }

  def lookup(info: TableInfo) : Option[SetValue[TupleValue]] = {
    database.get(info.table).map{case (columns, setv) => {
        val index = get_index(columns, info.columns)
        //projection
        setv.map{l: List[PrimitiveValue] => TupleValue(l.map(index))}
      }
    }
  }

}

object DataBase{

  def fromLoader(dl: DataLoader) : DataBase = {
    val database = dl.loaded_database
    new DataBase(database)
  }

}


class DataLoader(path: String){

  val source = Source.fromFile(path)

  val loaded_database = {
    val lines = source.getLines
    parse(lines, "", List(), Map(), Map())
  }

  private val table_exp : Regex = "(\w+):\n".r
  private val num_exp : Regex = "(-?\d+(\.\d*)?)".r

  private def isNum(s: String) = {
    s match{
      case num_exp(d, _) => true
      case _ => false
    }
  }

  private def parse(lines: List[String], table: String, tags: List[Tag], columns: List[Column], acc_map: Map[String, Set[List[PrimitiveValue]]]) : Map[String, Set[List[PrimitiveValue]]] = {
    //caseが3重になってるのいやだな。。。
   lines match{
     case y::yl =>{
       y match{
         case table_exp(p) =>{
           yl match {
             case t::c::ys => {
               val table_new = p
               //assuming length of tags and corresponnding columns are the same
               val tags_new = t.stripLineEnd.split("\t").toList
               val columns_new = c.stripLineEnd.split("\t").toList
               //there might be a table with no specific values
               val map_new = acc_map + (p->(columns_new, Set()))
               parse(ys, pre_new, tags_new, columns_new, map_new)
             }
             case _ => {println("invalid table detected at the bottom"); acc_map} //ignore invalid table and finish
           }
         }
        case _ =>{
          //assuming length of tags, columns and corresponding data are the same
          val v_new = y.stripLineEnd.split("\t").zip(tags).map {case (s, t) => if (isNum(s)) new NumericValue(s.toDouble, t) else SymbolicValue(s, t)}
          val set_new = acc_map.getOrElse(table, (List(), Set()))._2 + v_new
          val map_new = acc_map + (table->(columns, set_new))
          parse(yl, table, tags, columns, map_new)
        }
       }
     }
     //finish
     case _ => acc_map
   }
  }

}
