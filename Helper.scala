
object Helper
{
	
	// 3 rule class
	class RuleClass(var clause:ClauseClass , var body:List[ClauseClass])
	{
	}

	// 6 clause class
	class ClauseClass(var x:Any)
	{
		
	}
	// 7 predicate class
	class PredClass(var name:String, var arglist:List[ArgClass]) 
	{
		def toPredStr = name
	}

	// 9 arg class
	class ArgClass(x:Any)
	{
		def isInt() = { true } 
		def toInt() = {1}
		def toStr = 
		{
			x match 
			{
				case t:TermClass => t.asInstanceOf[TermClass].toTermStr()
				case v:VarClass => v.asInstanceOf[VarClass].toVarStr()
				case c:ConstClass => c.asInstanceOf[ConstClass].toConstStr()
				case i:Int => i.toString
				case s:String => s
			}
		}

	}

	// 10 term class
	class TermClass(var str:String) extends ArgClass
	{
		def toTermStr() = str
		def strtemp() = str
	}

	// 11 var class
	class VarClass(var str:String) extends ArgClass
	{
		def isVarClass() = { true }
		def toVarStr() = str
	}

	// 12 const class
	class ConstClass(var name:TermClass, var i:Int) extends ArgClass
	{
		def toConstStr() = name.strtemp()
		
	}




	/*class singleScript(_facts, _statements){
		def facts = _facts
		def statements = _statements
	}

	class clingoFact(_name, _parameterA, _parameterB){
		def factName = _name
		def paraA = _parameterA
		def paraB = _parameterB
	}*/

}
