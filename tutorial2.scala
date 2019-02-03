
object tutorial2 {
  
 
  def main(args: Array[String]) = {
    print (duplicated (List (0,1,2,2), 3 ) + "\n")
    print ( noDups (List (0,1,2,2)) + "\n" )
    print ( flength (List(0,1,2,3,4) ) + "\n" )
    print ( fcompress (List('a','a','a','a','b','c','c','a','a','d','e','e','e','e') ) + "\n" )
    print ( duplicateN ( 3, List(0,1,2,3,4) ) + "\n" )
    print ( flattenN ( List ( List(1,2), List(List(1,2),List(3,4)), 1, List(2,3) )) + "\n" )
    print (isSorted( List(1, 3, 5, 7), (x: Int, y: Int) => x <= y) + "\n")
    print (isSorted( List("a","a","ab","abc","abcd"), (x: String, y: String) => x.length() <= y.length() ) + "\n")
  }
  
  def duplicated[T](xs:List[T], x:T): Boolean = {
    xs.count( _ == x) > 1
  }
  
  def noDups[T](xs:List[T]): Boolean = {
    xs map ( x => duplicated (xs,x)  ) forall ( _ == false )  
  }
  
  def flength[T](xs:List[T]): Int = {
    xs.foldLeft(0)( (count , _ ) => count + 1)
  }
  
  def fcompress[T](xs:List[T]):List[T] = {
    xs.foldRight ( List[T](xs.last) ) ( (a, b) =>  if (a == b.head) b else a :: b  )
    
  }

  def duplicateN[T](n: Int, xs:List[T]): List[T] = {
    xs.flatMap ( List.fill(n)(_) ) 
  }
    
  def flattenN(xss:List[Any]): List[Any] = {
    xss.flatMap {case a:List[Any] => flattenN (a) case b => List(b) } 
  }

  def isSorted[A](xs:List[A], ord:(A, A) => Boolean): Boolean = {
    (xs zip xs.tail) forall ( x =>  ord  ( x._1 , x._2 )  ) 
   
 }
 
 

}

