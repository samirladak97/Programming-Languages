def shoelace(filename: String): Double={
  //import to be able to read from a file
  import scala.io.Source
  //read the file
  val file = Source.fromFile(filename, "UTF-8").getLines
  //make the file into a string
  val text = file.mkString
  //make the string into a string array of the integers in string format
  val stringarr = text.split("\\s+")
  //convert the string array into an int array
  val intarr = stringarr.map(x => x.toInt)
  //compute the first sum using recursion
  def firstsum(arr: Array[Int], index: Int=0): Double={
    if (index==arr.size) 0
    else arr(index)*arr((index+3)%arr.size) + firstsum(arr, (index+2))
  }
    //compute the second sum using recursion
  def secondsum(arr: Array[Int], index: Int=0): Double={
    if (index==arr.size) 0
    else arr((index+2)%arr.size)*arr(index+1) + secondsum(arr, (index+2))
  }
  //subtract first sum from second sum
  val dubarea = firstsum(intarr)-secondsum(intarr)
  //finish the formula by dividing by 2 and taking the absolute value
  val area = math.abs(dubarea/2)
  //return the final value of area
  area
}

