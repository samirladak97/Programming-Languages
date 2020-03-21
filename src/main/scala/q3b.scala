import scala.util.parsing.combinator._
class WMLParser2 extends RegexParsers {
  //all token values from q1
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

  //helper function to break down lists when terminal tokens are combined with non-terminals
  //list = list of tokens
  //inner = new nodes to be constructed
  //term = terminal token constructor
  //nonTerm = constructor for the new node created
  //this funtion returns a new ASTNode of type notTerm
  def listBreak(list: List[Any], inner: List[ASTNode], term: (String) => ASTNode, nonTerm: (List[ASTNode]) => ASTNode): ASTNode = {
    if(list.isEmpty) nonTerm(inner)
    else {
      list(0) match {
        case s: String => listBreak(list.tail, inner :+ term(s), term, nonTerm)
        case n: ASTNode => listBreak(list.tail, inner :+ n, term, nonTerm)
      }
    }
  }
  //<program>
  def program : Parser[ASTNode] = rep(Outertext | invoke | define) ^^ {
    case Nil => ASTProgram(Nil)
    case list => listBreak(list, Nil, ASTOutertext(_), ASTProgram(_))
  }
  //<invoke>
  def invoke : Parser[ASTNode] = Tstart ~ itext ~ targs ~ Tend ^^ {
    case _ ~ i ~ t ~ _ => ASTInvoke(List(i, t))
  }
  //<targs>
  def targs : Parser[ASTNode] = rep(Pipe ~ opt(itext) ^^ {
    case p ~ None => (p, null)
    case p ~ Some(i) => (p, i)
  }) ^^ {
    case Nil => ASTTargs(Nil)
    case list => {
      val inner = list.map((x) => x._2)
      ASTTargs(inner)
    }
  }
  //<itext>
  def itext : Parser[ASTNode] = rep(Inneritext | tvar | invoke | define) ^^ {
    case Nil => ASTItext(Nil)
    case list => listBreak(list, Nil, ASTInneritext(_), ASTItext(_))
  }
  //<tvar>
  def tvar : Parser[ASTNode] = Vstart ~ Vname ~ opt(Pipe ~ itext) ~ Vend ^^ {
    case _ ~ _ ~ None ~ _ => ASTTvar(Nil)
    case _ ~ _ ~ Some(Pipe ~ i) ~ _ => ASTTvar(List(i))
  }
  //<define>
  def define : Parser[ASTNode] = Dstart ~ dtextn ~ dparams ~ Pipes ~ dtextb ~ Dend ^^ {
    case _ ~ dtn ~ dp ~ _ ~ dtb ~ _ => ASTDefine(List(dtn, dp, dtb))
  }
  //<dtextn>
  def dtextn : Parser[ASTNode] = rep(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDtextn(Nil)
    case list => listBreak(list, Nil, ASTInnerdtext(_), ASTDtextn(_))
  }
  //<dparams>
  def dparams : Parser[ASTNode] = rep(Pipe ~ dtextp) ^^ {
    case Nil => ASTDparams(Nil)
    case list => {
      val children = list.map((x) => x._2)
      ASTDparams(children)
    }
  }
  //<dtextp>
  def dtextp : Parser[ASTNode] = rep1(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDtextp(Nil)
    case list => listBreak(list, Nil, ASTInnerdtext(_), ASTDtextp(_))
  }
  //<dtextb>
  def dtextb : Parser[ASTNode] = rep(Bodytext | invoke | define | tvar) ^^ {
    case Nil => ASTDtextb(Nil)
    case list => listBreak(list, Nil, ASTBodytext(_), ASTDtextb(_))
  }
}

object q3b {
  def main(args: Array[String]): Unit = {
    //import to be able to read from a file
    import scala.io.Source
    //read the file
    val file = Source.fromFile(args(0), "UTF-8").getLines
    //make the file into a string
    val text = file.mkString
    val p = new WMLParser()
    val test = p.parseAll(p.program, args(0))
    if (test.successful)
      println(test.get.toString)
  }
}

