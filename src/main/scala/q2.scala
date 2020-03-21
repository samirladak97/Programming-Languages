
import scala.io.Source
//import scala.reflect.runtime.universe
//import scala.tools.reflect.ToolBox

//create an environment class that will consist of a List of bindings and the parent environment
class Environment(){
  var binding = ""
  var parent: Environment = null
  var params: List[String] = null
  var body: ASTNode = null
}

object q2 extends App{
  def eval(a: ASTNode, e: Environment): String={
    a match{
      case ASTProgram(outer) =>
        //recursively evaluate each item in the initial ASTProgram using helpEval helper method defined below
        helpEval(0, outer, e)
      case ASTInvoke(name, targs) =>
        //create new environment and determine where the body will be executed based on the name in the Invoke
        val x = new Environment
        x.parent = e
        x.params = eval(targs, e).split("\\s+").map(_.trim).toList
        if (eval(name, e) == e.binding)
          eval(e.body, x)
        else if (eval(name, e.parent) == e.parent.binding)
          eval(e.parent.body, x)
        else if (eval(name, e.parent.parent) == e.parent.parent.binding)
          eval(e.parent.parent.body, x)
        else
          ""
      case ASTTargs(targs) =>
        //targs is a list of ASTNodes so we must recursively evaluate this list
        //use helpEval helper method defined below to do this
        helpEval(0, targs, e)
      case ASTItext(itext) =>
        //itext is a list of ASTNodes so we recursively evaluate using helpEval method
        helpEval(0, itext, e)
      case ASTTvar(name, opt) =>
        if (e.binding == name)
          eval(e.body, e)
        else if (e.parent.binding == name)
          eval(e.parent.body, e.parent)
        else if (e.parent.parent.binding == name)
          eval(e.parent.parent.body, e.parent.parent)
        else if (opt != null)
          eval(opt, e)
        else
          name
      case ASTTdef(name, dparams, body) =>
        //create a new environment and point it to the current environment
        val x = new Environment
        x.parent = e
        x.binding = eval(name, e)
        x.body = body
        //take the string of params given by case ADTDparams and convert it to a list
        x.params = eval(dparams, e).split("\\s+").map(_.trim).toList
        //after defining we just return the empty string
        ""
      case ASTDparams(dparams) =>
        helpEval(0, dparams, e)
      case ASTDtext(k, dtext) =>
        helpEval(0, dtext, e)
      //if we just have a token such as outertext, we just return the token
      case ASTText(k, s) => s
    }
  }
  //helper function to evaluate lists of ASTNodes recursively
  //parameters:
  //s: start point in the list where we want to start evaluating (0 in most cases)
  //x: the list we want to evaluate
  //e: the environment in which we want to evaluate
  def helpEval(s: Int, x: List[ASTNode], e: Environment): String={
    if (s==x.size) ""
    else eval(x(s) ,e) + helpEval(s+1, x, e)
  }

  //match input with the case that it is provided
  //source becomes a WML string
  //we get the root node of the WML input and put it through the above eval function
  val source = args match{
    case Array(_, "-p", fn) => Source.fromFile(fn, "UTF-8").mkString
    case Array("-p", fn) => Source.fromFile(fn, "UTF-8").mkString
    case Array(_, fn) => Source.fromFile(fn, "UTF-8").mkString
    case Array(fn) => Source.fromFile(fn, "UTF-8").mkString
  }
  val p = new WMLParser
  //print the evaluated version of the AST
  println(eval(p.parseAll(p.program, source).get, null))

}
