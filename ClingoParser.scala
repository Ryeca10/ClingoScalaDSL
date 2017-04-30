import scala.language.implicitConversions



object ClingoParser extends Regexparsers{
	//whitespace info as demostrated by Loc's Chefparser
	override def skipWhitespace = true;
	override val whiteSpace = "[ \t\r\f]+".r

	//Parser for a clingo file, made up of lists of each containing facts followed by statement.
	def clingoProgram: Parser[List[singleScript]] =
		clingoScript ~ (newLine ~> clingoScript).* ^^ {
			//lists of lists, looks like: Lists(List(), List(), ...)
			case r ~ l => r :: l 
		}
	//Parser for a single list of facts followed by a statemnent
	def clingoScript: Parser[singleScript] =
		factList.? ~ statemnent.+ ^^ {
			case None ~ _statements =>
				//have not implemented singleScript yet, will store
				//a single set of facts and statemnent
				new singleScript(List(), _statements)
			case _facts ~ _statements =>
				new singleScript(_facts, _statements)
			//continue cases if necessary...
		}
	//Parser for a list of facts
	def factList: Parser[List[clingoFact]] =
		fact.+
	//Parser for a single facts 
	def fact: Parser[clingoFact] =
		("""[A-Za-z]""".r <~ "(") ~ ( (number | word) <~ "," ) ~
		( (number | word) <~ ")" ) ^^ {
			case _name ~ Some(a) ~ Some(b) => 
				new clingoFact(_name, a, b)
			//continue cases if necessary...
		}
	//Parser for a single statement, made up of a predicate and followed by facts to check
	def statemnent: Parser[statemnent] =
		//Currently broken; predicate :- pfact; pfact (or) pfact;; works
		(predicate <~ ":-") ~ (pFact <~ ("," | ";")) ~ (pFact.? <~ ";") ^^ {

		}




	//Parser for a single predicate 
	def predicate: Parser[String] =
		("""[A-Za-z]""".r <~ "(" ) ~ (term <~ ")" ) ^^ {
			//check for some logic that implies that the term given is true
			if(true)
				return true;
		}





}



