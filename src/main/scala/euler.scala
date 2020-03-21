object euler extends App{
  println(euler(69))
  def euler(k: Int): Double = {
    //helper method to compute the sum
    def eulerSum(k: Int): Double = {
      if (k==0) 0
      else eulerSum(k-1) + (1/(math.pow(k, 2)))
    }
    //use helper method then simplify pi^2/6 to just pi and return estimate for pi
    math.sqrt(eulerSum(k)*6)
  }
}
