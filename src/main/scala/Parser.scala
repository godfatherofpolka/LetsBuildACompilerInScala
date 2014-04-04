sealed trait ParserResult[+T]
case class Success[+T](result : T, remainder : String) extends ParserResult[T]
case class Failure(msg : String) extends ParserResult[Nothing]

trait Parser[+T] {
  def parse(s : String) : ParserResult[T]

  def <=>(f: T => Boolean) : Parser[T] = new PredicateParser(this, f)

  def <|>[S >: T](that : Parser[S]) : Parser[S] = new ChoiceParser(this, that)

  def <+>[S](that : => Parser[S]) : Parser[(T,S)] = new SequenceParser(this, that)

  def <+->[S](that : => Parser[S]) : Parser[T] = new SequenceConsumeRightParser(this, that)

  def <-+>[S](that : => Parser[S]) : Parser[S] = new SequenceConsumeLeftParser(this, that)

  def >>>[S](f: T => S) : Parser[S] = new TransformParser(this, f)

  def >>>>[S](v: => S) : Parser[S] = new TransformUnitParser(this, v)

  def +>[S](f: T => Parser[S]) : Parser[S] = new BindParser(this, f)

  def iter : Parser[List[T]] = new IterateParser(this)
}

class PredicateParser[+T](p : => Parser[T], test : T => Boolean) extends Parser[T] {
  lazy val parser = p
  def parse(s : String) = parser parse s match {
    case f : Failure => f
    case Success(result, remainder) => 
      if (test(result)) 
        Success(result, remainder) 
      else 
        Failure(result + " does not match predicate in " + s)
  }
}

class ChoiceParser[T <: S, +S](p : => Parser[T], q : => Parser[S]) extends Parser[S] {
  lazy val first = p
  lazy val second = q
  def parse(s : String) = first parse s match {
    case Failure(msg) => second parse s
    case success@Success(result, remainder) => success
  }
}

class SequenceParser[+T,+S](p : => Parser[T], q : => Parser[S]) extends Parser[(T,S)] {
  lazy val first = p
  lazy val second = q
  def parse(s : String) = first parse s match {
    case f1 : Failure => f1
    case Success(t1, s1) => second parse s1 match {
      case f2 : Failure => f2
      case Success(t2, s2) => Success( (t1, t2), s2)
    }
  }
}

class SequenceConsumeRightParser[+T,+S](p : => Parser[T], q : => Parser[S]) extends Parser[T] {
  lazy val first = p
  lazy val second = q
  def parse(s : String) = new SequenceParser(first, second) parse s match {
    case f : Failure => f
    case Success((result1, result2), remainder) => Success(result1, remainder)
  }
}

class SequenceConsumeLeftParser[+T,+S](p : => Parser[T], q : => Parser[S]) extends Parser[S] {
  lazy val first = p
  lazy val second = q
  def parse(s : String) = new SequenceParser(first, second) parse s match {
    case f : Failure => f
    case Success((result1, result2), remainder) => Success(result2, remainder)
  }
}

class TransformParser[+T,+S](p : => Parser[T], transformer : T => S) extends Parser[S] {
  lazy val parser = p
  def parse(s: String) = parser parse s match {
    case f : Failure => f
    case Success(result, remainder) => Success(transformer(result), remainder)
  }
}

class TransformUnitParser[+T,+S](p : => Parser[T], v : => S) extends Parser[S] {
  lazy val parser = p
  def parse(s : String) = parser parse s match {
    case f : Failure => f
    case Success(_, remainder) => Success(v, remainder)
  }
}

class ResultParser[+T](x : => T) extends Parser[T] {
  def parse(s : String) = Success( x, s )
}

class BindParser[+T,+S](p : => Parser[T], transform : T => Parser[S]) extends Parser[S] {
  lazy val parser = p
  def parse(s : String) = parser parse s match {
    case f : Failure => f
    case Success(result, remainder) => transform(result) parse remainder
  }
}

class IterateParser[+T](p: => Parser[T]) extends Parser[List[T]] {
  lazy val parser = p
  lazy val empty = new ResultParser(Nil) >>>> Nil
  lazy val parseNext = (parser <+> new IterateParser(parser)) >>> { case (head, tail) => head :: tail }

  def parse(s : String) = (parseNext <|> empty).parse(s)

  def cons[T](tuple : Tuple2[T, List[T]]) : List[T] = tuple._1 :: tuple._2 
}

class Parsers {
  def char = new Parser[Char] {
    def parse(s : String) = s match {
      case "" => Failure("Expected character, empty string found")
      case _  => Success(s.charAt(0), s.drop(1))
    }
  }

  def literal(c : Char) = char <=> (_ == c)

  def literal(l : String) = new Parser[String] {
    def parse(s : String) =  {
      val n = l.length
      val candidate = s take n
      if (l equals candidate) 
        Success(l, s drop n)
      else
        Failure("Expected " + l + ", found " + candidate)
    }
  }

  def digit = char <=> Character.isDigit
  def letter = char <=> Character.isLetter
  def space = char <=> Character.isWhitespace
  def notSpace = char <=> (!Character.isWhitespace(_))
  def alphanum = digit <|> letter
  def digitVal = digit >>> (_.asDigit)
  def number = digitVal +> numberHelper
  def numberHelper(x : Int) : Parser[Int] = 
    ((digitVal >>> buildNumber(x)) +> numberHelper) <|> result(x)
  def buildNumber(x : Int)(y : Int) : Int = 10 * x + y
  def letters = ((letter.iter) >>> { _.mkString }) <=> ( (s : String) => !(s.equals("")) )
  def token[T](p : => Parser[T]) : Parser[T] = {
    lazy val parser = p
    (parser <+-> (space.iter))
  }
  def option[T](p : => Parser[T]) : Parser[Option[T]] = {
    lazy val parser = p
    (parser >>> { Some(_) }) <|> 
    (literal("") >>>> None)
  }
  def result[T](x : T) : Parser[T] = new ResultParser(x)
  def accept(w : String) = token(literal(w))
  def accept(c : Char) = token(literal(c))
  def accumulator[S](parseConnective : => Parser[S => (S => S)], parseNext : => Parser[S], build : S => ((S => (S => S), S)) => S) : S => Parser[S] = {
    (v : S) =>
      (((parseConnective <+> parseNext) >>> { build(v) }) +> accumulator(parseConnective, parseNext, build)) <|> result(v)
  }

}




