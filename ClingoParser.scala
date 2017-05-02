import scala.util.parsing.combinator._
import scala.language.implicitConversions
import ClingoClasses._
mport scala.collection.mutable.HashMap

// program     -->    rule + program    |     rule
// rule    -->    clause  + ":-"  +  body.
// body    -->     clause + "," + body    |    clause +  "."
// fact   -- > clause + "."
// clause   -->    predicate    |    comparison    |     boolean
// boolean    -->   true | false
// comparison    -->   subterm   +    relation   + subterm
// predicate    -->  lowercasename   +    "("   +   term   +   ")"
// arglist    -->   arg +   ","   +  arglist   |    arg  +   ";"   +  arglist   |  arg
// arg    -->    const (all lower case) |  variable (first letter caps)  |   positive integer  |   term (all lowercase)
// relation   -->      >=    |    <=    |    <     |   >   |    =     |   !=
// const   -->  "#" + "const" + " " + term + "=" + positive integer + "."
// term   -->   lower case name 
// variable  -->    first letter caps


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
 	//not sure if it can be a single clingoExpr
 	def clingoArg: Parser[Argument] = 
 		( clingoPredAssign | (clingoExpr <~ ",") | clingoExpr | clingoFactCall | clingoPrint | clingoBody ) {
 			case a => a
 		}

 	def clingoFactName: Parser[String] = """([a-z]+)""".r ^^ {
 		case vn => vn
 	}

 	def clingoPredAssign: Parser[Assign] = ((clingoFactName <~ "(") ~ (clingoExpr <~ ")")) | ((clingoFactName <~ "(") ~ (clingoExpr <~ "),"))^^ {
 		case f ~ e => Assign(f, e)
 	}
 	//def clingoFactDecl: Parser[Fact] = 

 	//combined decl and assign
 	def clingoFactAssign: Parser[Assign] = (clingoFactName <~ "(") ~ (clingoExpr <~ ").") ^^ {
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

 	def clingoPredDef: Parser[Expression] = 
 		clingoFactName ^^ {
 			case f => PredDef(f)
 		}

 	def clingoBoolean: Parser[Expression] =
 		case t => True()

 	def clingoNegate: Parser[Expression] = 
 		"not " ~> clingoExpr ^^ {
 			case n => Negation(n)
 		}

 	//def clingoRule: Parser[Rule] =
 	def clingoRuleCall: Parser[RuleCall] =
 		(clingoPredDef <~ "(") ~ (clingoExpr <~ ")") ~ clingoBody ^^ {
 			case fd ~ exp ~ b => RuleCall(fd, exp, b)
 		}

 	def clingoPrint: Parser[Show] = ("#show " ~> clingoPredDef) ~ ("/" ~> clingoNumber) ^^ {
 		case fd ~ i => Show(fd, i)
 	}

 } 


















































// object ClingoParser extends Regexparsers{
// 	//whitespace info as demostrated by Loc's Chefparser
// 	override def skipWhitespace = true;
// 	override val whiteSpace = "[ \t\r\f]+".r

// 	def stablemodels = new mutable.HashMap[String, String]


// 	def program: Parser[ProgClass] =
// 		(line ~ program) | line ^^ { 
// 			case l ~ p => new ProgClass(l, p)
// 			case l ~ None => new ProgClass(l, none)
// 		}
// 	def line: Parser[LineClass] =
// 		rule | fact | const ^^ {
// 			p match{
// 				case r: RuleClass => LineClass(p) //LineClass(arg: Any)
// 				case f: FactClass => LineClass(f)
// 				case c: ConstClass => LineClass(c)
// 			}
// 		}
// 	def rule: Parser[RuleClass] =
// 		(clause <~ ":-") ~ body ^^ {
// 				case None ~ b => new RuleClass(None, b) //if none check constraint in RuleClass
// 				case c ~ b => new RuleClass(c, b)
// 		}
// 	def body: Parser[BodyClass] =
// 		(clause ~ "," ~ body) | (clause ~ ".") ^^ {
// 			c ~ str ~ b => 
// 				str match{
// 					case "," => new BodyClass(str, c, b)
// 					case "." => new BodyClass(str, c, None)
// 				}

// 		}
// 	def fact: Parser[FactClass] =
// 		(term <~ "(") ~ (arg <~ ")")  ^^ {
// 			t ~ a =>
// 				a match{
// 					case _a: VarClass => throw new 
// 					RuntimeException("Unsafe arg of type VarClass: "+ a)
// 					case _ => FactClass(t, a) //make sure that t and a are places
// 											 // in stablemodels inside the class
// 				}
// 		}

// 	def const: Parser[Int] =
// 		("#const " ~> term) ~ ("=" ~> integer) <~ "." ^^ {

// 		}
// 	def clause: Parser[ClauseClass] =
// 		predicate | comparison | boolean ^^ {
// 			c match{
// 				case p: PredClass => 
// 					new ClauseClass(p)
// 				case c: CompClass =>
// 					new ClauseClass(c)
// 				case b: Boolean =>
// 					new ClauseClass(b)
// 			}

// 		}
// 	def boolean: Parser[Boolean] =
// 		"#true" | "#false" ^^ {
// 			b match{
// 				case "#true" => true
// 				case "#false" => false
// 			}
// 		}
// 	def comparison: Parser[Boolean] =
// 		arg ~ (">=" | "<=" | "<" | ">" | "=" | "!=") ~ arg ^^ {
// 			a1 ~ r ~ a2 =>
// 				r match{
// 					case ">=" => (a1: Int && a2:Int && a1 >= a2)
// 					//more cases to follow..
// 				}

// 		}
// 	def predicate: Parser[PredClass] =
// 		("""[a-z]""".r <~ "(") ~ (arglist <~ ")") ^^ {
// 			_name ~ _arglist =>
// 				new PredClass(_name, _arglist)
// 		}
// 	def arglist: Parser[ArgListClass] =
// 		((arg ~ str) ~ arglist) ^^ {
// 			a ~ s ~ alist =>
// 				s match {
// 					case "," => new ArgListClass(s, a, alist)
// 					case ";" => new ArgListClass(s, a, alist)
// 					case None => new ArgListClass(None, a, None)
// 				}

// 		}
// 	def arg: Parser[ArgClass] =
// 		const | variable | integer | term | intset ^^ {
// 			a match{
// 				case c: ConstClass =>
// 					new ArgClass(c) //Class ArgClass(arg: ArgClass) -arg classes extend ArgClass
// 				case v: VarClass =>
// 					new ArgClass(v)
// 				case i: Int =>
// 					new ArgClass(i)
// 				case t: TermClass =>
// 					new ArgClass(t)
// 				case a: Array[Int] =>
// 					new ArgClass(a)
// 				case _ => println("Oops no class match")
// 			}

// 		}
// 	// def relation: Parser[String] =
// 	// 	">=" | "<=" | "<" | ">" | "=" | "!=" ^^ {

// 	// 	}
// 	def term: Parser[TermClass] =
// 		"""[a-z]""".r ^^ {
// 		}
// 	def variable: Parser[VarClass] =
// 		"""([A-Z]+) ([A-Za-z]*)""".r ^^ {

// 		}
// 	def intset: Parser[Array[Int]] =
// 		((integer <~ "..") ~ integer) ^^ {

// 		}
// 	def integer: Parser[Int] =
// 		"""([0-9]+)""".r ^^ {
// 			_.toInt
// 		}






// }










// object ClingoParser extends Regexparsers{
// 	//whitespace info as demostrated by Loc's Chefparser
// 	override def skipWhitespace = true;
// 	override val whiteSpace = "[ \t\r\f]+".r

// 	//Parser for a clingo file, made up of lists of each containing facts followed by statement.
// 	def clingoProgram: Parser[List[singleScript]] =
// 		clingoScript ~ (newLine ~> clingoScript).* ^^ {
// 			//lists of lists, looks like: Lists(List(), List(), ...)
// 			case r ~ l => r :: l 
// 		}
// 	//Parser for a single list of facts followed by a statemnent
// 	def clingoScript: Parser[singleScript] =
// 		factList.? ~ statemnent.+ ^^ {
// 			case None ~ _statements =>
// 				//have not implemented singleScript yet, will store
// 				//a single set of facts and statemnent
// 				new singleScript(List(), _statements)
// 			case _facts ~ _statements =>
// 				new singleScript(_facts, _statements)
// 			//continue cases if necessary...
// 		}
// 	//Parser for a list of facts
// 	def factList: Parser[List[clingoFact]] =
// 		fact.+
// 	//Parser for a single facts 
// 	def fact: Parser[clingoFact] =
// 		("""[A-Za-z]""".r <~ "(") ~ ( (number | word) <~ "," ) ~
// 		( (number | word) <~ ")" ) ^^ {
// 			case _name ~ Some(a) ~ Some(b) => 
// 				new clingoFact(_name, a, b)
// 			//continue cases if necessary...
// 		}
// 	//Parser for a single statement, made up of a predicate and followed by facts to check
// 	def statemnent: Parser[statemnent] =
// 		//Currently broken; predicate :- pfact; pfact (or) pfact;; works
// 		(predicate <~ ":-") ~ (pFact <~ ("," | ";")) ~ (pFact.? <~ ";") ^^ {

// 		}




// 	//Parser for a single predicate 
// 	def predicate: Parser[String] =
// 		("""[A-Za-z]""".r <~ "(" ) ~ (term <~ ")" ) ^^ {
// 			//check for some logic that implies that the term given is true
// 			if(true)
// 				return true;
// 		}
// }



