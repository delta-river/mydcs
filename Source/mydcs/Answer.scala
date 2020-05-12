package mydcs

import java.io.PrintWriter

object Answer{
  private def tostr(deno: Option[Denotation]) : String = {
    deno match {
      case Some(feasible) => feasible.answer
      case None => "failed to answer: No feasible DCS Tree found"
    }
  }

  def output(base_path: String, answers: List[Option[Denotation]]) = {
    val line : String = answers.map(tostr).mkString("", "\n", "")

    val path = base_path + ".answer"
    val out = new PrintWriter(path)
    out.write(line)
    out.close()
  }
}
