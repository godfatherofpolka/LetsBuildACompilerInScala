case class Program(block : Block) extends Compileable

case class Block(statements : List[Statement]) extends Compileable

abstract class Statement extends Compileable
case class Assignment(variable : Variable, value : Expression) extends Statement
case class If(condition : BooleanExpression, block : Block) extends Statement
case class IfElse(condition : BooleanExpression, ifBlock : Block, elseBlock : Block) extends Statement
case class While(condition : BooleanExpression, block : Block) extends Statement
case object Break extends Statement

abstract class Expression extends Compileable
case class Numeral(value : Int) extends Expression
case class Variable(name : String) extends Expression
case class Add(l : Expression, r : Expression) extends Expression
case class Sub(l : Expression, r : Expression) extends Expression
case class Mul(l : Expression, r : Expression) extends Expression
case class Div(l : Expression, r : Expression) extends Expression

abstract class BooleanExpression extends Compileable
case object True extends BooleanExpression
case object False extends BooleanExpression
case class And(l : BooleanExpression, r : BooleanExpression) extends BooleanExpression
case class Or(l : BooleanExpression, r : BooleanExpression) extends BooleanExpression
case class Not(b : BooleanExpression) extends BooleanExpression
case class AsBoolean(e : Expression) extends BooleanExpression
case class Equals(l : BooleanExpression, r : BooleanExpression) extends BooleanExpression
case class LessThan(l : BooleanExpression, r : BooleanExpression) extends BooleanExpression
