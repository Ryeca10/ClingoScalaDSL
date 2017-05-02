import ClingoEval._

abstract class ClingoScript
case class Program(block: CodeBlock)
case class ArgSequence(args: List[Argument])

abstract class Argument extends ClingoScript