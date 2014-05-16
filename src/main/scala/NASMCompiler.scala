// TODO : very unfinished...
object NASMCompiler extends Compiler {
  var labelCounter = 0
  val variables = scala.collection.mutable.Set[String]()
  val constInitVariables = scala.collection.mutable.Map[String, Int]()

  def getLabel() = {val label = "L"+labelCounter; labelCounter +=1; label}

  override def compileProgram(block : Block) = {
    val textSection = compile(block)
    val bssSection = variables.map( (x : String) => format(x + " resw 1") ).mkString("")
    "section .bss\n" + bssSection + "\n" + "section .text\n" + textSection
  }

  def compileBlock(statements : List[Statement]) = {
    val compiledBlock = for(statement <- statements) 
                          yield compile(statement)
    (compiledBlock.mkString)
  }

  def compileStatement(statement : Statement) = (statement match {
    case Assignment(Variable(v), e) => {
      variables += v
      compile(e) + format("mov [" + v + "], eax" )
    }
    case If(c, b) => {
      val condition = compile(c) 
      val block = compile(b)
      val label = getLabel()
      condition +
      format("test eax, 0") +
      format("jne " + label) +
      block + 
      label + ":\n"
    }
    case IfElse(c, b1, b2) => ""
    case While(c, b) =>
      val whileLabel = getLabel()
      val condition = compile(c)
      val block = compile(b)
      val endWhileLabel = getLabel()
      whileLabel + ":\n" +
      condition +
      format("test eax, 0") +
      format("jne " + endWhileLabel) +
      block +
      format("jmp " + whileLabel) +
      endWhileLabel + ":\n"
    case Break => ""
    case other => throw new CompilerException("Can not compile " + other)
  }) + "\n"

  def compileBinaryExpression(first : Expression, second : Expression, op : String) =
    compile(first) +
    format("push eax") +
    compile(second) +
    format("pop ebx") +
    format(op + " eax, ebx")

  def compileExpression(expression : Expression) = expression match {
    case Numeral(n) => format("mov eax, " + n)
    case Variable(v) => format("mov eax,  [" + v + "]")
    case Add(x,y) => compileBinaryExpression(x, y, "add")
    case Sub(x,y) => compileBinaryExpression(x, y, "sub")
    case Mul(x,y) => compileBinaryExpression(x, y, "mul")
    case Div(x,y) => compileBinaryExpression(x, y, "div")
    case other => throw new CompilerException("Can not compile " + other)
  }

  def compileBooleanExpression(booleanExpression : BooleanExpression) = 
    booleanExpression match {
      case True => format("mov eax, 1")
      case False => format("mov eax, 0")
      case And(x,y) => compileBinaryBooleanExpression(x, y, "and")
      case Or(x,y) => compileBinaryBooleanExpression(x, y, "or")
      case Not(x) => compile(x) + format("not eax")
      case Equals(x,y) => compileBooleanRelation(x, y, "je")
      case AsBoolean(e) => compile(e)
      case LessThan(x,y) => compileBooleanRelation(x, y, "jl")
      case other => throw new CompilerException("Can not compile " + other)
    }

 def compileBinaryBooleanExpression(first : BooleanExpression, second : BooleanExpression, op : String) =
    compile(first) +
    format("push eax") +
    compile(second) +
    format("pop ebx") +
    format(op + " eax, ebx")

  def compileBooleanRelation(first : BooleanExpression, second : BooleanExpression, op : String) = {
    val evaluation = compile(first) +
                     format("push eax") +
                     compile(second) +
                     format("pop ebx") +
                     format("test eax, ebx")
    val trueLabel = getLabel()
    val endLabel = getLabel()
    format(op + " " + trueLabel) +
    compile(False) + 
    format("jmp " + endLabel) +
    trueLabel + ":\n" +
    compile(True) +
    endLabel + ":\n"
  }

  def format(s : String) = "\t" + s + "\n"
}
