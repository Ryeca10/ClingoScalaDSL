import ClingoEval._

//CodeBlock = Body
abstract class ClingoScript
case class Program(codeBlock: Body)
case class ArgSequence(args: List[Argument])

abstract class Argument extends ClingoScript
case class Rule(name: String, vars: List[Strings], codeBlock: Body) extends Argument
case class Fact(name: String, pOne: String) extends Argument
case class Assign(name: String, expr: Expression) extends Argument
case class Show(expr: Expression) extends Argument
case class Body(argumentSeq: List[Argument]) extends Argument

abstract class Expression extends Argument
case class RuleCall(name: String, params: List[Expression]) extends Expression
case class FactDef(name: String, pOne: String) extends Expression
case class Number(i: Integer) extends Expression
case class Str(s: String) extends Expression
case class True() extends Expression
case class Negation(expr: Expression) extends Expression

object Parser extends RegexParsers{
	override def skipWhitespace = true

	def clingoProgram: Parser[Program] = 
		body ^^ {
			case b => Program(b)
		}
 	
 	def clingoBody: Parser[Body] =
 		":-" ~> clingoArgSeq <~ "." ^^ {
 			case a => Body(a)
 		}

 	def clingoArgSeq = clingoArg*

 	//clingoFact = varDecl
 	//can include comments
 	def clingoArg: Parser[Argument] = 
 		( (clingoExpr <~ ",") | clingoExpr | clingoFact | clingoFactCall | clingoPrint | clingoBody ) {
 			case a => a
 		}

 	def clingoFactName: Parser[String] = """([a-z]+)""".r ^^ {
 		case vn => vn
 	}

 	//def clingoFactDecl: Parser[Fact] = 

 	//combined decl and assign
 	def clingoFactAssign: Parser[Assign] = (clingoFactName <~ "(") ~ (clingoExpr <~ ")") ^^ {
 		case f ~ e => Assign(f, e)
 	}

 	def clingoExpr: Parser[Expression] = clingoRuleCall | clingoNumber | clingoString ^^ {
 		case exp => exp
 	}

 	def comment = "%.*%".

 	def clingoString: Parser[Expression] =
 		"\"" ~> """([^"]*)""".r <~ "\"" ^^ {
 			case str => Str(str)
 		}

 	def clingoNumber: Parser[Expression] = 
 		"""([0-9]+)""".r ^^ {
 			case i => Number(i)
 		}

 	def clingoFactDef: Parser[Expression] = 
 		clingoFactName ^^ {
 			case f => FactDef(f)
 		}

 	def clingoBoolean: Parser[Expression] =
 		case t => True()

 	def clingoNegate: Parser[Expression] = 
 		"not " ~> clingoExpr ^^ {
 			case n => Negation(n)
 		}

 	//def clingoRule: Parser[Rule] =
 	def clingoRuleCall: Parser[RuleCall] =
 		(clingoFactDef <~ "(") ~ (clingoExpr <~ ")") ~ clingoBody ^^ {
 			case fd ~ exp ~ b => RuleCall(fd, exp, b)
 		}

 	def clingoPrint: Parser[Show] = ("#show " ~> clingoFactDef) ~ ("/" ~> clingoNumber) ^^ {
 		case fd ~ i => Show(fd, i)
 	}

 } 






















