object stringPipeline extends App{
  val p = stringPipeline("lTr")
  println(p("Daddy 69"))
  def stringPipeline(s: String) = (x: String) => {
    //helper function that will take the conversion string and the input string
    //recursively perform the tasks for each char in the conversion string
    def func(t: String, p: String = s): String= {
      if (p.startsWith("U")) {
        func(t.toUpperCase, p.replace("U", ""))
      }
      else if (p.startsWith("l")) {
        func(t.toLowerCase, p.replace("l", ""))
      }
      else if (p.startsWith("T")) {
        func(t.split(' ').map(_.capitalize).mkString(" "), p.replace("T", ""))
      }
      else if (p.startsWith("r")) {
        func(t.reverse, p.replace("r", ""))
      }
      else if (p.startsWith("s")) {
        func(t.sorted, p.replace("s", ""))
      }
      else if (p.startsWith("*")) {
        func(t.replaceAll(" ", ""), p.replace("*", ""))
      }
      else t
    }
    //return the helper function which returns a string
    func(x, s)
  }
}
