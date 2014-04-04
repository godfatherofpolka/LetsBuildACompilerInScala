/* to run use
 >sbt run
 */

// based on http://alephnullplex.appspot.com/blog/view/2010/02/24/lbach-7-parser-combinators
// and http://www.codecommit.com/blog/scala/the-magic-behind-parser-combinators
// and https://github.com/alephnullplex/cradle/tree/master/src/Cradle

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val programCode = if (args.size != 0) {
      Source.fromFile(args(0)).mkString
    } else {
      Source.fromURL(getClass.getResource("/testProgram.txt")).mkString
    }
    val program = ProgramParser.parse(programCode)
    println(TrivialCompiler.compile(program))
    println(LLVMCompiler.compile(program))

  }
}
