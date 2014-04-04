trait Compileable {
  def compile(c : Compiler) : String = c.compile(this)
}

class CompilerException(msg: String) extends RuntimeException(msg)

trait Compiler {
  def compile(o : Compileable) : String = o match {
    case Program(block) => compileProgram(block)
    case Block(ss) => compileBlock(ss) 
    case s : Statement => compileStatement(s)
    case e : Expression => compileExpression(e)
    case b : BooleanExpression => compileBooleanExpression(b)
    case other => throw new CompilerException("Can not compile " + other)
  }
  
  def programHeader = ""
  def programFooter = ""
  def compileProgram(block : Block) = programHeader + compile(block) + programFooter
  def compileBlock(statements : List[Statement]) : String
  def compileStatement(statement : Statement) : String
  def compileExpression(expression : Expression) : String
  def compileBooleanExpression(booleanExpression : BooleanExpression) : String
}
