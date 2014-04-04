// to compile: fsc src/AbstractSyntax.scala src/Compiler.scala src/Main.scala src/Parser.scala src/ProgramParser.scala -d ./bin

// based on http://alephnullplex.appspot.com/blog/view/2010/02/24/lbach-7-parser-combinators
// and http://www.codecommit.com/blog/scala/the-magic-behind-parser-combinators
// and https://github.com/alephnullplex/cradle/tree/master/src/Cradle

object Main {
  def main(args: Array[String]) {
    if (args.size != 0) {
      val programCode = io.Source.fromFile(args(0)).mkString
      val program = ProgramParser.parse(programCode)
      println(TrivialCompiler.compile(program))
      println(LLVMCompiler.compile(program))
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
