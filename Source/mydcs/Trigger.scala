package mydcs

import scala.io.Source
import scala.util.matching.Regex

//laod lexical triggers from lexical trigger file
//style of lexical trigger file *.trig is as below
//pred_name1:word1\tword2\tword3...\n
//...

class LexicalTrigger(base_path: String){

  // no error handling
  private val path = base_path + ".trig"
  private val source = Source.fromFile(path)

  //map from words to predicate name
  private val lextrigger : Map[String, String] = {
    val lines = source.getLines
    load(lines.toList, Map[String, String]())
  }

  def lookup(s: String) : Option[String] = lextrigger.get(s)

  private val trigger_exp = """(.+):(.+)\n""".r

  private def load(lines: List[String], acc_map: Map[String, String]) : Map[String, String] = {
    lines match{
      case y::yl => {
        y match{
          case trigger_exp(pred_name, words) =>{
            val word_list = words.split("\t")
            // overwrite if there are words that corresponds to some different predicate name
            val map_new = word_list.foldLeft(acc_map){ case (l, w) => l + (w->pred_name)}
            load(yl, map_new)
          }
          //ignore line of just "\n"
          case "\n" => load(yl, acc_map)
          case _ => {
            println("!!!!!!!!!!!!!!!!!trigger ignored: style miss-match")
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
