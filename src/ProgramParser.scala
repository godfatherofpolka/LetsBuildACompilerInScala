class ProgramParserException(msg: String) extends RuntimeException(msg)

object ProgramParser extends Parsers {
  def parse(s : String) : Program = program.parse(s) match {
    case Success(p, "") => p
    case Failure(msg) => throw new ProgramParserException(msg)
  }

  def program : Parser[Program] = 
    accept("program") <-+> block <+-> accept("end") >>> { Program(_) }

  def block : Parser[Block] = (statement iter) >>> { Block(_) }

  def statement : Parser[Statement] = 
    assignStatement <|>
    ifStatement <|>
    ifElseStatement <|>
    whileStatement <|>
    breakStatement

  def assignStatement : Parser[Assignment] = 
    (variable <+-> token(literal('=')) <+> expression) >>> { x =>  Assignment(x._1, x._2) }

  def ifStatement : Parser[If] =
    accept("if") <-+> condition <+> block <+-> accept("end") >>>
    { (x : Tuple2[BooleanExpression, Block]) => If(x._1, x._2) }

  def ifElseStatement : Parser[IfElse] =
    accept("if") <-+> condition <+> block <+-> accept("else") <+> block <+-> accept("end") >>>
    { (x : Tuple2[Tuple2[BooleanExpression, Block],Block]) => IfElse(x._1._1, x._1._2, x._2) }

  def whileStatement : Parser[While] =
    accept("while") <-+> condition <+> block <+-> accept("end") >>>
    { (x : Tuple2[BooleanExpression, Block]) => While(x._1, x._2) }

  def breakStatement : Parser[Statement] =
    accept("break") >>>> { Break }

  def condition : Parser[BooleanExpression] = booleanExpression

  def expression : Parser[Expression] = 
    token(term) +> accumulator(addOp, term, buildOp(_))

  def term : Parser[Expression] = 
    token(factor) +> accumulator(mulOp, factor, buildOp(_))

  /*def expression : Parser[Expression] = 
    token(term) +> expressionHelper
  def expressionHelper(e : Expression) : Parser[Expression] = 
    (((addOp <+> term) >>> buildOp(e) ) +> expressionHelper) <|> 
    result(e)

  def term : Parser[Expression] = 
    token(factor) +> termHelper
  def termHelper(e : Expression) : Parser[Expression] = 
    (((mulOp <+> factor) >>> buildOp(e) ) +> termHelper) <|> 
    result(e)*/
  
  def factor : Parser[Expression] = 
     parens(expression) <|>
     numeral <|> 
     variable

  def numeral : Parser[Numeral] = 
    token(number) >>> { Numeral(_) }

  def variable : Parser[Variable] = 
    token(letters) >>> { Variable(_) }

  def parens[S](p : => Parser[S]) : Parser[S] =
    token(lparen <-+> token(p) <+-> rparen)

  def lparen = accept('(')
  def rparen = accept(')')

  def buildOp[S](a: S)(p : (S => (S => S), S)) : S = {
    val op = p._1
    val b = p._2
    (op(a))(b)
  }

  def plusOp = accept('+')
  def minusOp = accept('-')
  def multOp = accept('*')
  def divOp = accept('/')

  def addOp : Parser[Expression => (Expression => Expression)] = 
   (plusOp <|> minusOp) >>> { pickOp(_) }

  def mulOp : Parser[Expression => (Expression => Expression)] = 
   (multOp <|> divOp) >>> { pickOp(_) }

  def pickOp(c : Char) : Expression => (Expression => Expression) = 
    (x : Expression) => ( (y : Expression) => c match {
      case '+' => Add(x,y)
      case '-' => Sub(x,y)
      case '*' => Mul(x,y)
      case '/' => Div(x,y)
    })

  def booleanExpression : Parser[BooleanExpression] =
    token(booleanFactor) +> accumulator(booleanOp, booleanFactor, buildOp(_))

  /*def booleanExpression : Parser[BooleanExpression] = 
    token(booleanFactor) +> booleanExpressionHelper
  def booleanExpressionHelper(e : BooleanExpression) : Parser[BooleanExpression] =
    (((booleanOp <+> booleanFactor) >>> buildOp(e)) +> booleanExpressionHelper) <|>
    result(e)*/

  def booleanFactor : Parser[BooleanExpression] = 
    parens(booleanExpression) <|>
    relationExpression <|>
    ((booleanNot <+> booleanExpression) >>> { (x : Tuple2[BooleanExpression => BooleanExpression, BooleanExpression]) => x._1(x._2) })<|>
    booleanLiteral

  def parensBooleanExpression = 
    token(lparen <-+> token(booleanExpression) <+-> rparen)

  def relationExpression : Parser[BooleanExpression] =
    asBoolean +> accumulator(relationOp, asBoolean, buildOp(_))

  /*def relationExpression : Parser[BooleanExpression] =
    booleanExpression +> relationExpressionHelper
  def relationExpressionHelper(e : BooleanExpression) : Parser[BooleanExpression] =
    (((relationOp <+> booleanExpression) >>> buildOp(e)) +> relationExpressionHelper) <|>
    result(e)*/
  
  def asBoolean : Parser[BooleanExpression] =
    token(expression) >>> { AsBoolean(_) }

  def booleanNot : Parser[BooleanExpression => BooleanExpression] = 
    notOp >>>> { (x : BooleanExpression) => Not(x) }

  def booleanLiteral : Parser[BooleanExpression] =
    (accept("true") >>>> { True }) <|>
    (accept("false") >>>> { False })
 
  def andOp = accept("&&")
  def orOp = accept("||")
  def notOp = accept('!')
  def equalsOp = accept("==")
  def lessThanOp = accept("<")

  def booleanOp : Parser[BooleanExpression => (BooleanExpression => BooleanExpression)] =
    (andOp <|> orOp) >>> { pickBooleanOp(_) }

  def relationOp : Parser[BooleanExpression => (BooleanExpression => BooleanExpression)] =
    ( equalsOp <|> lessThanOp ) >>> { pickBooleanOp(_) }

  def pickBooleanOp(op : String) : BooleanExpression => (BooleanExpression => BooleanExpression) = 
    (x : BooleanExpression) => ( (y : BooleanExpression) => op match {
      case "&&" => And(x,y)
      case "||" => Or(x,y)
      case "==" => Equals(x,y)
      case "<"  => LessThan(x,y)
    })
}
