import scala.language.implicitConversions



object ClingoParser extends Regexparsers{
	//whitespace info as demostrated by Loc's Chefparser
	override def skipWhitespace = true;
	override val whiteSpace = "[ \t\r\f]+".r

	def clingoProgram: Parser[List[singleScript]] =
		clingoScript ~ (newLine ~> clingoScript).* ^^ {
			case r ~ l => r :: l 
		}
	def clingoScript: Parser[singleScript] =
		factList.? ~ predicate.+ ^^ {
			case None ~ _predicates =>
				//have not implemented singleScript yet, will store
				//a single set of facts and predicates
				new singleScript(List(), _predicates)
			case _facts ~ _predicates =>
				new singleScript(_facts, _predicates)
			//continue cases if necessary...
		}
	def factList: Parser[List[singleFacts]] =
		fact.+
	def fact: Parser[String] =
		"""[A-Za-z]""".r ~ "(" ~ (number | word) ~ "," ~
		(number | word) ~ ")." ^^ {
		}





}



def predicate: Parser[String] =
	"""[A-Za-z]""".r~"("~term~")" ^^ {
		if(term)
			return true;
	}