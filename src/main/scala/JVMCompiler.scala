// TODO : very unfinished...
object JVMCompiler extends Compiler {
  var lineNumber = 0
  var index = 0
  val localIndex = scala.collection.mutable.Map[String,String]()

  def compileProgram(block : Block) = ""

  def compileBlock(statements : List[Statement]) = {
    val compiledBlock = for(statement <- statements) 
                          yield compile(statement)
    (compiledBlock.mkString)
  }

  def compileStatement(statement : Statement) = (statement match {
    case Assignment(v, e) => {
      if (!localIndex.contains(v.name))
        {localIndex += (v.name -> index.toString); index += 1;}
      compile(e) + lineNumber + ": " + "istore " + localIndex(v.name)
    }
    case If(c, b) => ""
    case IfElse(c, b1, b2) => ""
    case While(c, b) => ""
    case Break => ""
    case other => throw new CompilerException("Can not compile " + other)
  }) + "\n"

  def compileBinaryExpression(first : Expression, second : Expression, op : String) = ""

  def compileExpression(expression : Expression) = expression match {
    case Numeral(v) => "sipush " + v
    case Variable(n) => "iload " + localIndex(n)
    case Add(x,y) => compileBinaryExpression(x, y, "add")
    case Sub(x,y) => compileBinaryExpression(x, y, "sub")
    case Mul(x,y) => compileBinaryExpression(x, y, "mul")
    case Div(x,y) => compileBinaryExpression(x, y, "sdiv")
    case other => throw new CompilerException("Can not compile " + other)
  }

  def compileBooleanExpression(booleanExpression : BooleanExpression) = 
    booleanExpression match {
      case True => ""
      case False => ""
      case And(x,y) => compileBinaryBooleanExpression(x, y, "and")
      case Or(x,y) => compileBinaryBooleanExpression(x, y, "or")
      case Not(x) => compileBinaryBooleanExpression(x, True, "xor")
      case Equals(x,y) => compileBooleanRelation(x, y, "eq")
      case AsBoolean(e) => compile(e)
      case LessThan(x,y) => compileBooleanRelation(x, y, "slt")
      case other => throw new CompilerException("Can not compile " + other)
    }

 def compileBinaryBooleanExpression(first : BooleanExpression, second : BooleanExpression, op : String) = ""

  def compileBooleanRelation(first : BooleanExpression, second : BooleanExpression, op : String) = ""
}
