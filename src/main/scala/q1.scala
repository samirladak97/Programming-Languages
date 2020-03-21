//Samir Ladak
//260667874

import scala.io.Source
object q1 extends App {
  //prettyPrint function matches cases based on the node that needs to be printed and
  //recursively calls itself until the WML code is printed in a more readable format
  def prettyPrint(a: ASTNode): String ={
    a match{
      case ASTProgram(outer) => helper(0, outer).trim
      case ASTInvoke(name, targs) => "{{" + wsremover(prettyPrint(name), "l") + wsremover(prettyPrint(targs),"r") + "}}"
      case ASTTargs(targs) => pipehelp(0, targs).trim
      case ASTItext(itext) => helper(0, itext).trim
      case ASTTvar(name, opt) => "{{{" + name.trim + {if (opt == null) "" else "|" + wsremover(prettyPrint(opt), "r")} + "}}}"
      case ASTTdef(name, dparams, body) => "{'" + prettyPrint(name).trim + wsremover(prettyPrint(dparams), "r") + "||" + prettyPrint(body) + "'}"
      case ASTDparams(dparams) => pipehelp(0, dparams).trim
      case ASTDtext(k, dtext) => helper(0, dtext)
      case ASTText(k, s) => s
    }
  }

  //helper method allows the pretty printer to recurse through and easily print names and text
  //without removing internal whitespace
  def helper(s: Int, x: List[ASTNode]): String={
    if (s==x.size) ""
    else prettyPrint(x(s)) + helper(s+1, x)
  }

  //pipehelp is similar to helper except it adds a pipe before
  //the pipe is important to use when pretty printing arguments and parameters
  def pipehelp(s:Int, x: List[ASTNode]): String={
    if (s==x.size) ""
    else "|" + prettyPrint(x(s)).trim + pipehelp(s+1, x)
  }

  //wsremover method is to help remove whitespace from only one side of a string (ie. left side or right side)
  def wsremover(s: String, side: String): String ={
    if (side=="l") s.replaceAll("^\\s+", "")
    else if (side=="r") s.replaceAll("\\s+$", "")
    else s
  }

  //match input with the case that it is provided
  //source becomes a WML string
  //we get the root node of the WML input and put it through the above prettyPrint function

  val source = "{{{abc} }}"
  /*val source = args match{
    case Array(_, "-p", fn) => Source.fromFile(fn, "UTF-8").mkString
    case Array("-p", fn) => Source.fromFile(fn, "UTF-8").mkString
  }*/
  val p = new WMLParser
  //print the final "pretty" version of the AST
  println(prettyPrint(p.parseAll(p.program, source).get))

}
