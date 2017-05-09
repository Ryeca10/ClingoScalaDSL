import scala.collection.mutable.ListBuffer
import ClingoParser._
import Helper._

object Runner
{
	def main(args: Array[String]) 
	{
		val parser = ClingoParser
		val lines = scala.io.Source.fromFile("ClingoCode").mkString
		val result = parser.parse(parser.program, lines)
		println("Parse result: " + result);
        println("stable models is empty: " + parser.stablemodels.isEmpty)

        var outputString = ""
		parser.stablemodels.foreach
		{
			case (key,value) =>
			{
                var smodelstring = key + "("
                value.foreach
                {
                    case b=>
                    {
                        smodelstring = smodelstring.concat(b.toStr + ",")
                    }
                }
                smodelstring = smodelstring.dropRight(1)
                smodelstring = smodelstring.concat(")")
                outputString = outputString.concat(smodelstring + ", ")
			}
		}
        outputString = outputString.dropRight(1)
        outputString = outputString.dropRight(1)
        println(outputString)
	}

}
