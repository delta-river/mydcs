package mydcs

import scala.io.Source
import scala.util.matching.Regex

class InputTree(val root: (String, String), val children: List[InputTree])

//style of input tree is as bellow
//({root.text|root.lemma};\tchild1\tchild2...)

object InputTree{
  //private val tree_exp = """\({([^\(\)\{\};\|]+)\|([^\(\)\{\};\|]+)};\t?(.*)\)""".r

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
    val tree_exp = """\(\{([^\(\)\{\};\|]+)\|([^\(\)\{\};\|]+)};\t?(.*)\)""".r
    line match{
      case tree_exp(root_text, root_lemma, children) =>{
        val str_children : List[String] = split_brackets(children).map(_.trim)
        Some(new InputTree((root_text, root_lemma), str_children.flatMap(parse)))
      }
      case _ => println("!!!!!!!!!!!!!!!!!!tree miss match!!!!!!!!!!!!!!!!!!!");println(line);None
    }
  }

  //better use Either to inform error
  def load(base_path: String) : List[Option[InputTree]] = {
    val path : String = base_path + ".tree"
    val source = Source.fromFile(path)
    // # for comment out
    val lines = source.getLines.filterNot{s: String => (s.length == 0 || s.startsWith("#"))}
    lines.toList.map(parse)
  }

}

class MediateTree(val root: Predicate, val children: List[MediateTree])

class DCSTree(val root: Predicate, val children : List[(Relation, DCSTree)]){
  //DCS Tree output style is as below
  //("root_name";\t{Rel1Child1}....)
  def output : String = {
    val this_pred = "\"" + root.name + "\";"
    val child_str = children.map{case (r, t) => "{" + r.toString + t.output + "}"}
    (this_pred +: child_str).mkString("(", "\t", ")")
  }
}

object DCSTree{
  import java.io.PrintWriter
  private def tostr(tree: Option[DCSTree]) : String = {
    tree match{
      case Some(feasible) => feasible.output
      case None => "No feasible DCS Tree found"
    }
  }
  def output(base_path: String, trees: List[Option[DCSTree]]) : Unit = {
    val line : String = trees.map(tostr).mkString("", "\n", "")

    val path = base_path + ".dcs"
    val out = new PrintWriter(path)
    out.write(line)
    out.close()
  }
}
