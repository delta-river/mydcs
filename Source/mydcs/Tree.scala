package mydcs

import scala.io.Source
import scala.util.matching.Regex

class InputTree(val root: (String, String), val children: List[InputTree])

//style of input tree is as bellow
//({root.text|root.lemma};\tchild1\tchild2...)

object InputTree{
  private val tree_exp = """\({([^\(\)\{\};\|]+)\|([^\(\)\{\};\|]+)};\t?(.*)\)""".r

  private def bracket_end(s: String) : List[Int] = {
    s.zipWithIndex.foldLeft((0, 0, List[Int]())){case (l, (r, i)) =>
      r match{
        case '(' => (l._1 + 1, l._2, l._3)
        case ')' => {
          val l2 = l._2 + 1
          if (l._1 == l2) (0, 0, l._3 :+ i) else (l._1, l2, l._3)
        }
        case _ => l
      }
    }._3
  }

  private def split_brackets(line: String) : List[String] = {
    val ends = bracket_end(line)
    ends.foldLeft((List[String](), 0)){case ((l, start), r) =>
      val s_new = line.slice(start, r+1)
      (l :+ s_new, r+1)
    }._1
  }

  private def parse(line: String) : Option[InputTree] = {
    //this matching might be skipped assuming fine input tree 
    line match{
      case tree_exp(root_text, root_lemma, children) =>{
        val str_children : List[String] = split_brackets(children).map(_.trim)
        Some(new InputTree((root_text, root_lemma), str_children.flatMap(parse)))
      }
      case _ => println("!!!!!!!!!!!!!!!!!!tree miss match!!!!!!!!!!!!!!!!!!!");println(line);None
    }
  }

  def load(base_path: String) : List[InputTree] = {
    val path : String = base_path + ".tree"
    val source = Source.fromFile(path)
    val lines = source.getLines.map(_.stripLineEnd)
    lines.toList.flatMap(parse)
  }

}

class MediateTree(val root: Predicate, val children: List[MediateTree])

class DCSTree(val root: Predicate, val children : List[(Relation, DCSTree)])
