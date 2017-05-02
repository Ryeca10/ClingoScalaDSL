import ClingoEval._

//CodeBlock = Body
abstract class ClingoScript
case class Program(code: Body)
case class ArgSequence(args: List[Argument])

abstract class Argument extends ClingoScript
case class Rule(name: String, vars: List[Strings], listOfArgs: Body) extends Argument
case class Fact(name: String, pOne: String) extends Argument
case class Pred(name: String, pOne: String) extends Argument
case class Assign(name: String, expr: Expression) extends Argument
case class Show(expr: Expression) extends Argument
case class Body(argumentSeq: List[Argument]) extends Argument

abstract class Expression extends Argument
case class RuleCall(name: String, params: List[Expression]) extends Expression
case class FactDef(name: String, pOne: String) extends Expression
case class PredDef(name: String, pOne: String) extends Expression
case class Number(i: Integer) extends Expression
case class Str(s: String) extends Expression
case class True() extends Expression
case class Negation(expr: Expression) extends Expression






















