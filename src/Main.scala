// to compile: fsc AbstractSyntax.scala Compiler.scala Main.scala Parser.scala ProgramParser.scala

object Main {
  def main(args: Array[String]) {
    if (args.size != 0) {
      val programCode = io.Source.fromFile(args(0)).mkString
      val program = ProgramParser.parse(programCode)
      println(TrivialCompiler.compile(program))
    } /*else {
   
      println(whileStatement.parse("while true\nz = y\ny = x + y\nx = z\nend"))
      println(breakStatement.parse("break"))
      println(booleanExpression.parse("!(i==0)"))
      println(assignStatement.parse("longName = value + 1324 * ( 15 + green )"))
      println(booleanFactor.parse("true"))
      println(booleanFactor.parse("!false"))
      println(booleanFactor.parse("1+3*4"))
   
    } */
  }
}
