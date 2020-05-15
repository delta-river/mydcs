package mydcs

import Types._

import scala.io.Source
import scala.util.matching.Regex

//prepare database which includes tables
//database style is as below
//table1:\n
//tag1\ttag2...\n
//column1\tcolumn2...\n
//value0.1\tvalue0.2...\n
//value1.1\tvalue1.2...\n
//...

class TableInfo(val table: String, val columns: List[Column])

//type Data = (List[Column], Set[List[PrimitiveValue]])
class DataBase(val database: Map[String, Data]){

  private def get_index(source: List[Column], index: List[Column]) : List[Int] = {
    index.map{ c: Column => source.indexWhere(_ == c)}
  }

  def lookup(info: TableInfo) : Option[Set[TupleValue]] = {
    database.get(info.table).map{case (columns, setv) => {
        val index = get_index(columns, info.columns)
        //projection
        //what to do with empty table, table which has no values
        //for now just take its as empty set, but sigleton set of empty TupleValue is better?
        setv.map{l: List[PrimitiveValue] => TupleValue(index.map(l))}
      }
    }
  }

  //should be careful not to set same name for custompred
  //probably this can be checked when the namepred table and custompred table are merged
  //or how about just adding : to the end of the namepred name ?
  //for : is not supposed to be used for custompred name
  def name_pred : Set[NamePredicate] = {
    def value2namePred(v: PrimitiveValue) : Option[NamePredicate] = {
      v match{
        case SymbolicValue(name, t) => Some(NamePredicate(name, t))
        case _ => None
      }
    }

    database.values.foldLeft(Set[NamePredicate]()){case (l, (_, setv)) => l | setv.flatMap{l: List[PrimitiveValue] => l.flatMap(value2namePred)}}
  }

  def get_abst(info: TableInfo) : Option[AbstractTup] = {
    database.get(info.table).map{case (columns, setv) => {
      val index = get_index(columns, info.columns)
      // take a list of vlaues from set
      // empty table not allowed
      val alist = setv.head
      val abst_list : List[AbstractValue] = index.map(alist).map{case SymbolicValue(_, t) => AbstractSymb(t) case NumericValue(_, t) => AbstractNum(t)}
      AbstractTup(abst_list)
      }
    }
  }

}

object DataBase{

  def fromLoader(dl: DataLoader) : DataBase = {
    val database = dl.loaded_database
    new DataBase(database)
  }

  def fromPath(path: String) : DataBase = {
    val dl = new DataLoader(path)
    val database = dl.loaded_database
    new DataBase(database)
  }

}


class DataLoader(base_path: String){

  // no error handling
  private val path = base_path + ".data"
  private val source = Source.fromFile(path)

  val loaded_database = {
    //lines does not contain "\n" at the last
    //remove line that only contains "\n"
    // # for comment out
    val lines = source.getLines.filterNot{s: String => (s.length == 0 || s.startsWith("#"))}
    //better to use Iterator than List(for a big file)
    //future work
    parse(lines.toList, "", List[Tag](), List[Column](), Map[String, Data]())
  }

  //ここにおくとなぜかぬるぽになる
  //private val table_exp : Regex = """(.+):\n""".r
  //private val num_exp : Regex = """(-?\d+(?:\.\d*)?)""".r

  private def isNum(s: String) = {
  val num_exp : Regex = """(-?\d+(?:\.\d*)?)""".r
    s match{
      case num_exp(d) => true
      case _ => false
    }
  }

  //type Data = (List[Column], Set[List[PrimitiveValue]])
  private def parse(lines: List[String], table: String, tags: List[Tag], columns: List[Column], acc_map: Map[String, Data]) : Map[String, Data] = {
    //caseが3重になってるのいやだな。。。
   val table_exp : Regex = """(.+):""".r
   lines match{
     case y::yl =>{
       y match{
         //new table start
         case table_exp(p) =>{
           yl match {
             case t::c::ys => {
               val table_new = p
               //assuming length of tags and corresponnding columns are the same
               val tags_new = t.split("\t").toList
               val columns_new = c.split("\t").toList
              //empty table not allowed
               parse(ys, table_new, tags_new, columns_new, acc_map)
             }
             case _ => {println("invalid table detected at the bottom"); acc_map} //ignore invalid table and finish
           }
         }
         //table values
        case _ =>{
          //assuming length of tags, columns and corresponding data are the same
          val v_new = y.split("\t").zip(tags).map {case (s, t) => if (isNum(s)) new NumericValue(s.toDouble, t) else SymbolicValue(s, t)}.toList
          val set_new = acc_map.getOrElse(table, (columns, Set[List[PrimitiveValue]]()))._2 + v_new
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
