package edu.library.classic

object Fibonacci{

  /*
   *  Case 1: Pattern Matching
   *  - Well suited for small numbers
   *  - If n is big, throws Stack Overflow
   *  - Gets really slow for n > 40 approx.
   */
  
  def fib1(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib1(n - 1) + fib1(n - 2)
  }
  
  /*
   * Case 2: Loop
   * - Handles integer numbers (32 bit)
   * - Too verbose, non-idiomatic
   */
  
  def fib2(n: Int): Int = {
    
    var first = 0
    var second = 1
    var count = 0
    
    while(count < n){
      val sum = first + second
      first = second
      second = sum
      count = count + 1
    }
    
    return first
  }
  
  /*
   * Case 3: Tail Recursion
   * - Optimized by compiler
   */
  
  def fib3(n: Int): Int = {
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n - 1, b, a + b)
    }  
    return fib_tail(n, 0 , 1)
  }
  
  /*
   * Case 4: Memoization
   * - Substitute 185 with amount of numbers to print
   * - Not suitable for big amounts (190 max approx.)
   */
  val fib: Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)

  def main(args: Array[String]){
    val s = fib take 185 mkString " "
    print(s)
    println()
    print(fib(180))
  }
  
  /*
   * Extra Case: Pisano period
   * - Get last 6 digits of Fibonacci with tail recursion
   */
  
  def fib5( n : Int) : Int = { 
    def fib_tail( n: Int, a:Int, b:Int): Int = n match {
      case 0 => a 
      case _ => fib_tail( n-1, b, (a+b)%1000000 )
    }
    return fib_tail( n%1500000, 0, 1)
  }
}