
package Tutorial

object tutorial4 {
  
 
  def main(args: Array[String]) = {
    
    /* demonstrate the behaviour of your functions 
    by calling them on suitable arguments 
    and printing out the results */
    odds.take(6).print 
    print( "\n")
    powers.take(6).print 
    print( "\n")
    repeats.take(20).print 
    print( "\n")
    factorialStream(3).take(6).print
    print( "\n")
    pascalStream(List (1 )).take(6).print
    print( "\n")
    interleave (odds,powers).take(12).print
  }
  
  val odds = Stream.from(1).filter(_%2 !=0)
  val powers = Stream.from(1).map(x => Math.pow(2,x))
  val repeats = Stream.from(1).flatMap (List.fill(3)(_))
 
 
  def factorialStream(i:Int): Stream[Int] = {
    def factorial(n: Int): Int = n match {
      case 0 => 1
      case _ => n * factorial(n-1)
    }  
    factorial(i) #:: factorialStream(i+1)
  }
 
  def nextRow(r: List[Int]): List[Int] = {
    val r1 = List (0) ::: r
    val r2 = r ::: List(0)
    (r1 zip r2 ) map ( x=> x._1 + x._2)
 }
 
  def pascalStream(r:List[Int]): Stream[List[Int]] = {
    r #:: pascalStream(nextRow(r))
  }
  
  def interleave[A](s:Stream[A], t:Stream[A]): Stream[A] = {
   s.head #:: t.head #:: interleave ( s.tail,t.tail  )
  } 
 
 
 /*
  def powersOfTwo(i:Int): Stream[Int] = i #:: powersOfTwo(i*2)
  val powers = powersOfTwo(1)
   
  def fibonacciStream(a:Int, b:Int): Stream[Int] = a #:: fibonacciStream(b, a+b)
  val fibs = fibonacciStream(1,1)
 
  def primeStream(s: Stream[Int]): Stream[Int] = s.head #:: primeStream(s.tail filter(_ % s.head != 0)) 
  val primes = primeStream(Stream.from(2))
  */
 
 
 
 
 
 
}