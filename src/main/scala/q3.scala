def fib2(n: Int): Int = {
  //helper method that does a tail recursion which is faster than normal recursion
  def fibHelp(x: Int, prev: Int=0, next: Int=1): Int ={
    if (x==0) prev
    else if (x==1) next
    else fibHelp(x-1, next, (next+prev))
  }
  //use helper method to compute
  //use n+1 as parameter because we want fib2(0) = 1, not 0, to match fib() method in assignment
  fibHelp(n+1)
}
