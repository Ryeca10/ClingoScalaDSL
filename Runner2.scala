import scala.collection.mutable.ListBuffer
<<<<<<< HEAD
import ClingoParser._
import Helper._
=======

class Runner(file: clingoFile){
	val text = file

	def run = {
		while 

>>>>>>> 3e9f20e1d6b05ba2cdd4d82d1a291f6e47e8ce70


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
