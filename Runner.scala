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
		println(parser.stablemodels.isEmpty)
		parser.stablemodels foreach 
		{ 
			case (key,value) => 
			{
				println(">>> key=" + key + ", value=" + value.mkString(", "))
				//println(">>> key=" + key)
				//for(_ <- value)
				//{
						//println(_.toStr)
					//case b =>
					//{
					//	b match
					//	{
							//case i:Int => println(i)
							//case s:String => println(s)
					//		 case _:ArgClass => println(_.toStr)
							//println(">>> key=" + key + ", value=" + value.mkString(", "))
				
					//	}
					//}
				//}
			}
		}
	}

}
