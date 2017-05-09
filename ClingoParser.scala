import scala.util.parsing.combinator._
import scala.language.implicitConversions
import scala.collection.mutable.HashMap
import Helper._
import scala.collection.mutable.ListBuffer



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


object ClingoParser extends RegexParsers
{
    //whitespace info as demostrated by Loc's Chefparser
    override def skipWhitespace = true;
    //override val whiteSpace = "[ \t\r\f]+".r

    var stablemodels = new HashMap[String, ListBuffer[ArgClass]]
    var constList = new HashMap[String, Int]

    def program: Parser[Boolean] = (line ~ program.?)  ^^
    {
        case l ~ p => {true}
        case _ => throw new RuntimeException(" Broken program. ");
        true
    }

    def line: Parser[Boolean] = (rule | fact | const) ^^
    {
        case p =>
        p match
        {
            case r: RuleClass => {true}
            case c: ConstClass => {true}
            case f: Boolean => {true}
            case _ => throw new RuntimeException(" Broken line of code. ")
        }
        true
    }

    def rule: Parser[RuleClass] = (clause <~ ":-") ~ (body <~ ".") ^^
    {
        case c ~ b => new RuleClass(c, b)
    }

    def body: Parser[List[ClauseClass]] = (clause ~ (",".?) ~ body.?) ^^
    {
        case c ~ None ~ None =>
        {
            var listOfClauses = new ListBuffer[ClauseClass]()
            listOfClauses += c
            listOfClauses.toList;
        }

        case c ~ str ~ b =>
        {
            var listOfClauses = new ListBuffer[ClauseClass]()
            if (str != null)
            {
                listOfClauses += c
                listOfClauses.insertAll( listOfClauses.size -1, b.get)
            }
            listOfClauses.toList;
        }
    }

    def fact: Parser[Boolean] = (term <~ "(") ~ (arglist <~ ").")   ^^
    {
        case t ~ alist =>
        {
            for (i <- 0 to alist.size-1)
            {
                var listtemp =  new ListBuffer[ArgClass]()
                listtemp.insertAll(0, alist)
                listtemp = listtemp.reverse
                stablemodels += (t.toTermStr() -> listtemp)
            }
            true;
        }

    }

    def const: Parser[ConstClass] = ("#const " ~> term) ~ ("=" ~> integer) <~ "." ^^
    {
        case t ~ i => { new ConstClass(t,i)}
    }

    def clause: Parser[ClauseClass] = (predicate | comparison | boolean) ^^
    {
        c => new ClauseClass(c)
    }

    def boolean: Parser[Boolean] = ("#true" | "#false") ^^
    {
        case b =>
        b match
        {
            case "#true" => true
            case "#false" => false
        }
    }

    def comparison: Parser[Boolean] = arg ~ (">=" | "<=" | "<" | ">" | "=" | "!=") ~ arg ^^
    {
        case a1 ~ r ~ a2 =>

        r match
        {
                case ">=" => ((a1.isInt()) && (a2.isInt()) && (a1.toInt >= a2.toInt))
                //more cases to follow..
        }

    }


    def predicate: Parser[PredClass] = ("""([a-z]+)""".r <~ "(") ~ ( (arglist) <~ ")") ^^
    {
        case _name ~ _arglist => new PredClass(_name, _arglist)
    }

    def arglist: Parser[List[ArgClass]] = (arg ~ (",".?) ~ arglist.?) ^^
    {
        case a ~ None ~ None =>
        {
            var listOfArgs = new ListBuffer[ArgClass]()
            listOfArgs += a
            listOfArgs.toList;
        }

        case a ~ s ~ alist =>
        {
            var listOfArgs = new ListBuffer[ArgClass]()
            if (s != null)
            {
                listOfArgs += a
                listOfArgs.insertAll( listOfArgs.size -1, alist.get)
            }
            listOfArgs.toList;
        }
    }

    def arg: Parser[ArgClass] = (const | variable | integer | term | intset) ^^
    {
            case a =>
            a match
            {
                case c:ConstClass => new ArgClass(c)
                case v:VarClass => new ArgClass(v)
                case i:Int => new ArgClass(i)
                case t:TermClass => new ArgClass(t)
                case intarr:Array[Int] => new ArgClass(intarr)
            }

    }

    def term: Parser[TermClass] = """([a-z]+)""".r ^^
    {
        x =>
        var str = x.toString
        new TermClass(str)
    }

    def variable: Parser[VarClass] = """([A-Z]+)""".r ^^
    {
        case x => {println("in variable"); new VarClass(x)}
    }

    def intset: Parser[Array[Int]] = ((integer <~ "..") ~ integer) ^^
    {
        case a ~ b =>
        var intArray = new Array[Int](b-a+1)
        var count = 0

        for( i <- a to b)
        {
            intArray(count) = i
            count = count + 1
        }

        intArray;

    }

    def integer: Parser[Int] = "[0-9]+".r ^^
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

/******************************
//case a ~ None ~ None =>
//{
//var listOfArgs = new ListBuffer[ArgClass]()
//listOfArgs += a
//listOfArgs.toList;
//}
for (i <- 0 to alist.size-1)
{
alist(i) match
{
case a:VarClass => throw new RuntimeException("Unsafe arg of type VarClass: " + alist(i).toString )
}
}


def fact: Parser[Boolean] = (term <~ "(") ~ (arglist <~ ").")   ^^
{
case t ~ alist =>
{
//println("in fact")

************************************/
/******************************
for (i <- 0 to alist.size-1)
{

var listtemp1 =  new ListBuffer[ArgClass]()
if (stablemodels.contains(t.toTermStr()))
{
listtemp1.insertAll( 0, stablemodels(t.toTermStr()))
listtemp1.insertAll(listtemp1.size - 1, alist)
var listtemp2 = new ListBuffer[ArgClass]()
for(j <- 0 to listtemp1.size-1)
{
if (!listtemp2.contains(listtemp1(j)))
{   listtemp2 += listtemp1(j)}
}
stablemodels(t.toTermStr()) = listtemp2

}
else
{
listtemp1.insertAll(0, alist)
stablemodels += (t.toTermStr() -> listtemp1)
}

}
true;
}

}

************************************/
