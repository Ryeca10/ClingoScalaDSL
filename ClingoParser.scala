import scala.util.parsig.combinator._
import scala.language.implicitConversions
import scala.collection.mutable.HashMap
import Helper._

// program     -->    rule + program    |     rule
// rule    -->    clause  + ":-"  +  body.
// body    -->     clause + "," + body    |    clause +  "."
// fact   -- > clause + "."
// clause   -->    predicate    |    comparison    |     boolean
// boolean    -->   true | false
// comparison    -->   subterm   +    relation   + subterm
// predicate    -->  lowercasename   +    "("   +   arglist   +   ")"
// arglist    -->   arg +   ","   +  arglist   |    arg  +   ";"   +  arglist   |  arg
// arg    -->    const|  variable | integer  |   term 
// relation   -->      >=    |    <=    |    <     |   >   |    =     |   !=
// const   -->  "#" + "const" + " " + term + "=" + positive integer + "."
// term   -->   lower case name 
// variable  -->    first letter caps


object ClingoParser extends Regexparsers
{
	//whitespace info as demostrated by Loc's Chefparser
	override def skipWhitespace = true;
	override val whiteSpace = "[ \t\r\f]+".r

	def stablemodels = new mutable.HashMap[String, String]
	

	def program: Parser[Boolean] = (line ~ program)  ^^ 
	{ 
			case l ~ p => true
			case l ~ None => true
			case _ => throw new RuntimeException(" Broken program. ");
			true
	}

	def line: Parser[Boolean] = rule | fact | const ^^ 
	{
			p match{
				case r: RuleClass => true 
				case f: Boolean => true
				case c: ConstClass => true
				case _ => throw new RuntimeException(" Broken line of code. ")
			}
			true
	}

	def rule: Parser[RuleClass] = (clause <~ ":-") ~ body ^^ 
	{
				case None ~ b => new RuleClass(None, b) //if none check constraint in RuleClass
				case c ~ b => new RuleClass(c, b)
	}

	def listOfClauses = new List()
	def body: Parser[List[ClauseClass]] = (clause ~ (","|".") ~ body) ^^ 
	{
		c ~ str ~ b => 

		if (str == ",")
		{
			 listOfClauses = (listOfClauses + c + b)
		}
		else if ( c != null) 
		{
			listOfClauses = (listOfClauses + c)
		}
		listOfClauses;



	}

	def fact: Parser[Boolean] = (term <~ "(") ~ (arglist <~ ").")   ^^ 
	{
		t ~ alist =>
	
		alist.foreach
		{	
			
			_: VarClass => throw new RuntimeException("Unsafe arg of type VarClass: "+ a)
		}
		
		alist.foreach
		{
			stablemodels(t.toString, _.toString)
		}
		
		true;
		
	}

	def const: Parser[ConstClass] = ("#const " ~> term) ~ ("=" ~> integer) <~ "." ^^ 
	{
		t ~ i =>
		ConstClass(t,i)
	}

	def clause: Parser[ClauseClass] = predicate | comparison | boolean ^^ 
	{
		c =>
 		ClauseClass(c)
	}

	def boolean: Parser[Boolean] = "#true" | "#false" ^^ 
	{
			b match
			{
				case "#true" => true
				case "#false" => false
			}
	}

	def comparison: Parser[Boolean] = arg ~ (">=" | "<=" | "<" | ">" | "=" | "!=") ~ arg ^^ 
	{
			a1 ~ r ~ a2 =>

			r match
			{
				case ">=" => (a1: Int && a2:Int && a1 >= a2)
				//more cases to follow..
			}

	}

	
	def listOfArgs = new List()
	def predicate: Parser[PredClass] = ("""[a-z]""".r <~ "(") ~ ( (arglist) <~ ")") ^^ 
	{
			_name ~ _arglist =>
			listOfArgs = new List()
			PredClass(_name, _arglist)
	}

	def arglist: Parser[List[ArgClass]] = (arg ~ ( "," | None) ~ arglist) ^^ 
	{
		a ~ s ~ alist =>

		if (s != null)
		{
			 listOfArgs = (listOfArgs + a + alist)
		}

		else if (a != null)
		{
			listOfArgs = (listOfArgs + a)
		}
		else
		{
			 throw new RuntimeException(" Empty list in arglistOR.")
		}

		listOfArgs;
	}

	def arg: Parser[ArgClass] = const | variable | integer | term | intset ^^ 
	{
		x => 
		ArgClass(x)

	}

	def term: Parser[TermClass] = """[a-z]""".r ^^ 
	{
		x =>
		var str = x.toString
		TermClass(str)
			
	}

	def variable: Parser[VarClass] = """([A-Z]+) ([A-Za-z]*)""".r ^^ 
	{
		x =>
		var str = x.toString
		VarClass(str)
	}

	def intset: Parser[Array[Int]] = ((integer <~ "..") ~ integer) ^^ 
	{
		a ~ b =>
		var intArray = new Array[Int](b-a+1)
		Int count = 0

		for( i <- a to b)
		{
			intArray[count] = i
			count = count + 1
		}

		intArray;

	}

	def integer: Parser[Int] = """([0-9]+)""".r ^^ 
	{
		_.toInt
	}
}


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

/*a match
			{
				case c: ConstClass =>
					new ArgClass(c) //Class ArgClass(arg: ArgClass) -arg classes extend ArgClass
				case v: VarClass =>
					new ArgClass(v)
				case i: Int =>
					new ArgClass(i)
				case t: TermClass =>
					new ArgClass(t)
				case a: Array[Int] =>
					new ArgClass(a)
				case _ => println("Oops no class match")
			}
*/

	// def relation: Parser[String] =
	// 	">=" | "<=" | "<" | ">" | "=" | "!=" ^^ {

	// 	}

