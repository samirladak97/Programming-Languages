object x extends App {
  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])+".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])+".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])+".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])+".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])+".r
}