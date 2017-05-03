import scala.collection.mutable.ListBuffer
import ClingoParser._
import Helper._


class Runner(file: "ClingoCode")
{
	val lines = scala.io.Source.fromFile("file.txt").mkString

	def main(args: Array[String]) 
	{
		println("Started program")
		val parser = ClingoParser
		println("created parser")
		val result = parser.parse(parser.program, lines)
		println("Parse result: " + result);
	}

}
