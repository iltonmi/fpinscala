package myanswers.gettingstarted

object GettingStarted {
  // 2.1
  def fib(n :Int) : Int = {
    def go(n : Int) : Int = {
      if(n <= 2) n
      else return fib(n-1) +fib(n-2)
    }
    go(n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n :Int) : Boolean = {
      if(n > as.length - 2) true
      else if(!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }
}
