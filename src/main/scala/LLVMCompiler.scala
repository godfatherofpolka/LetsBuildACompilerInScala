object LLVMCompiler extends Compiler {
  val assignmentStack = scala.collection.mutable.Stack[String]()

  var tmpId = 0

  def indent = "  "

  def getTmpId(name : String) = {tmpId += 1; name + tmpId}

  override def programHeader = "define i32 @main() {\n"

  override def programFooter = "}"

  def compileBlock(statements : List[Statement]) = {
    val compiledBlock = for(statement <- statements) 
                          yield (indent + compile(statement))
    (compiledBlock.mkString)
  }

  def compileStatement(statement : Statement) = (statement match {
    case Assignment(v, e) => {
      val expressionCode = compile(e)
      "%" + v + " = " + assignmentStack.pop()
    }
    case If(c, b) => {
      val conditionCode = compile(c)
      val label = getTmpId("if")
      val labelTrue = label + "_true"
      val labelNext = label + "_next"
      val blockCode = compile(b)
      conditionCode + 
        "br i1 " + assignmentStack.pop() + ", label %" + labelTrue + ", label %" + labelNext +
        labelTrue + ":\n" + 
        blockCode +
        labelNext + ":\n"
    }
    case IfElse(c, b1, b2) => ""
    case While(c, b) => ""
    case Break => ""
    case other => throw new CompilerException("Can not compile " + other)
  }) + "\n"

  def compileBinaryExpression(first : Expression, second : Expression, op : String) = {
      val firstCode = compile(first);
      val firstId = assignmentStack.pop()
      val secondCode = compile(second);
      val secondId = assignmentStack.pop()
      val tmpId = "%" + getTmpId(op + "tmp");
      assignmentStack.push(tmpId)
      tmpId + " = " + op + " i32 " + firstId + ", " + secondId
  }

  def compileExpression(expression : Expression) = expression match {
    case Numeral(v) => {assignmentStack.push(v.toString); assignmentStack.top}
    case Variable(n) => {assignmentStack.push("%" + n); assignmentStack.top}
    case Add(x,y) => compileBinaryExpression(x, y, "add")
    case Sub(x,y) => compileBinaryExpression(x, y, "sub")
    case Mul(x,y) => compileBinaryExpression(x, y, "mul")
    case Div(x,y) => compileBinaryExpression(x, y, "div")
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
