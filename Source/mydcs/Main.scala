package mydcs

//main entry point
object Mydcs{
  //style of args is as bellow
  //input_base_path output_base_path
  //input_base_path is used for database, lexical trigger, custom predicate definition
  //output_base_path is used for inputting trees, output results

  def main(args: Array[String]) : Unit = {
    if (args.length < 2) println("!!!!!!!!!!!!!!!!!!!!!!too few arguments!!!!!!!!!!!!!!!!!!!!") else {
      val input_base : String = args(0)
      val output_base : String = args(1)

      val db : Database = DataBase.fromPath(input_base)
      val world : World = new World(db)
      val cv : Converter = Converter.from(input_base, db)

      val dcs_trees : List[Option[DCSTree]] = InputTree.load(out_path).map(cv.convert(_))
      val answers : List[Option[Denotation]] = dcs_trees.map(_.map(Denotation.calculate(_, world)))
      Answer.output(output_base, answers)
    }
  }
}
