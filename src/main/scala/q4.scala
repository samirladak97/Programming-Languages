def cakes(n: Int): Boolean ={
  //set boolean values for mod arithmetic to simplify if statements
  val modtwo: Boolean = (n%2==0)
  val modthree: Boolean = (n%3==0)
  val modfour: Boolean = (n%4==0)
  val modfive: Boolean = (n%5==0)
  val x = n%100
  val y = x%10
  val z = (x-y)/10
  //if n is 40 then the right amount of cakes were returned
  if (n==40) true
  //if n is divisible by 2 return n/2 cakes only if you are still left with 40 or more cakes
  else if (modtwo && ((n/2)-40 >= 40)){
    val a = n - (n/2)
    cakes(a)
  }
  //if n is divisible by 3 or 4 then return cakes equal to the last 2 digits multiplied together
  //only do this if you will be left with 40 or more cakes
  else if ((modthree || modfour) && (n-y*z >= 40) && (y*z >= 1)){
    val a = n-y*z
    cakes(a)
  }
  //if n is divisible by 5 then return 40 cakes only if you are still left with 40 or more cakes
  else if (modfive && (n-40 >= 40)){
    val a = n-40
    cakes(a)
  }
  //last option: ending with exactly 40 cakes is impossible so return false
  else false
}
