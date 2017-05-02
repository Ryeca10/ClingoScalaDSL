import ClingoParser._
import scala.io.Source



object ClingoEval {
	var inside: Option[Level] = None 
	class Level{outterLevel: Option[Level]} {
		var outside: Option[Level] = outterLevel
		//in the form of 
		var models: List[Predicate] = new List[Predicates]
		var rules: HashMap[String, (List[String], Body)] =
			new HashMap[String, (List[String], Body)]()
		var preds: HashMap[String, Predicate] = new HashMap[String, Predicate]

		def setRule(predName: String, variables: List[Strings], listOfArgs: body){
			rules += (predName -> (variables, listOfArgs))
		}

		def getRule(predName: String): (List[String], Body) = {
			if(rules.contains(predName)){
				rules.get(name) match{
					case Some(r) => r
					//should we include exception cases?
				}
			}
		}
		//don't think we need defPred

		//is Unit necessary?
		def setPred(predName: String, predVal: Predicate): Unit = {
			if(preds.contains(predName)){
				preds(predName) = predVal
			}
			else{
				preds += (name -> predName)
				preds(predName) = predVal
			}
			// 	outside match{
			// 		case Some(level) => level.setPred(predName, predVal)

			// 	}
			// }
		}
		//for predicates outside of the rule, AKA facts
		def setFact(predName: String, predVal: Predicate): Unit = {
			outside match{
				case Some(level) => level.setPred(predName, predVal)
			}
		}

		def getPred(predName: String): Predicate = {
			if(preds.contains(predName)){
				rules.get(name) match{
					case Some(r) => r
					//should we include exception cases?
				}
			}
		}

		def getFact(predName: String): Predicate = {
			outside match{
				case Some(level) => level.getPred(predName)
			}
		}

	}

	abstract sealed class Predicate
	case class ClingoStr(predVal: String) extends Predicate
	//not sure if we need none yet
	case class ClingoNone() extends Predicate

	abstract sealed class Statement
	case class StateDecl() extends Statement
	case class StateAssign() extends Statement
	case class StateShow() extends Statement

	def evalProgram(prog: Program): Any = {

	}

	def evalBody(prog: Program): Predicate = {

	}

	//decl and call may be one single one
	def evalRuleDecl(ruleDecl: Rule): ClingoNone = {

	}

	def evalRuleCall(ruleCall: RuleCall): Predicate = {

	}

	def evalArgSeq(argSeq: List[Argument]): Predicate = {

	}

	//may need one for facts too
	def evalPredDecl(predDecl: Pred): ClingoNone = {

	}

	def evalPredDecl












}





