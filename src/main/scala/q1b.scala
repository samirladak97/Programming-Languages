import scala.util.parsing.combinator._
class bla {
  val Tstart = "{{"
  val Tend = "}}"
  val Vstart = "{{{"
  val Vend = "}}}"
  val Dstart = "{'"
  val Dend = "'}"
  val Pipe = "|"
  val Pipes = "||"
  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])+".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])+".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])+".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])+".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])+".r
}
