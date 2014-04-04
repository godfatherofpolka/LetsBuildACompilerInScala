object TrivialCompiler extends Compiler {
  var indentLevel = 0

  def indent = "  " * indentLevel

  override def programHeader = "program\n"

  def compileBlock(statements : List[Statement]) = {
    indentLevel += 1
    val compiledBlock = for(statement <- statements) 
                          yield (indent + compile(statement))
    indentLevel -= 1
    (compiledBlock.mkString) + indent + "end"
  }

  def compileStatement(statement : Statement) = (statement match {
    case Assignment(v, e) => compile(v) + " = " + compile(e) 
    case If(c, b) => "if " + compile(c) + "\n" + compile(b)
    case IfElse(c, b1, b2) => "if " + compile(c) + "\n" + compile(b1) + "else\n" + compile(b2)
    case While(c, b) => "while " + compile(c) + "\n" + compile(b)
    case Break => "break\n"
    case other => throw new CompilerException("Can not compile " + other)
  }) + "\n"

  def compileExpression(expression : Expression) = expression match {
    case Numeral(v) => v.toString
    case Variable(n) => n
    case Add(x,y) => "(" + compile(x) + "+" + compile(y) + ")"
    case Sub(x,y) => "(" + compile(x) + "-" + compile(y) + ")"
    case Mul(x,y) => "(" + compile(x) + "*" + compile(y) + ")"
    case Div(x,y) => "(" + compile(x) + "/" + compile(y) + ")"
    case other => throw new CompilerException("Can not compile " + other)
  }

  def compileBooleanExpression(booleanExpression : BooleanExpression) = 
    booleanExpression match {
      case True => "true"
      case False => "false"
      case And(x,y) => "(" + compile(x) + "&&" + compile(y) + ")"
      case Or(x,y) => "(" + compile(x) + "&&" + compile(y) + ")"
      case Not(x) => "!(" + compile(x) + ")"
      case Equals(x,y) => compile(x) + "==" + compile(y)
      case AsBoolean(e) => compile(e)
      case LessThan(x,y) => compile(x) + "<" + compile(y)
      case other => throw new CompilerException("Can not compile " + other)
    }
}
