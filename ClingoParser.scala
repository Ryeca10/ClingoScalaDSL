def predicate: Parser[String] =
	"""[A-Za-z]""".r~"("~term~")" ^^ {
		if(term)
			return true;
	}