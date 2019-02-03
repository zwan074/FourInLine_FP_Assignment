

object Tutorial1 {
  
  def main(args: Array[String]) = {
    
    print ( second (List(1,2,3,4)) + "\n")
    print ( middle (List(1,2,3,4,5)) + "\n" )
    print ( heads (List(3,2), List(5,6,7), List(1,1,1,1)) + "\n" )
    print ( sum (List(1,2,3,4)) + "\n" )
    print ( product(List(1,2,3,4,5)) + "\n" )
    print ( joinLists (List(List(1,0,0), List(0,1,0), List(0,0,1)) ) + "\n")
    print ( compress (List('a','a','a','a','b','c','c','a','a','d','e','e','e','e') ) )
    
  }
  
  def second[T](xs:List[T]): T = {
   xs.tail.head
  }
  
  def middle[T](xs:List[T]): List[T] = {
    if (xs.length > 2) xs.init.tail
    else List()
  }
  
  
  def heads(xs:List[Int], ys:List[Int], zs:List[Int]): List[Int] = {
    List(xs.head , ys.head , zs.head)  
  }
  
  def sum(xs:List[Int]): Int = {
    xs match {
      case List() => 0
      case x :: xs1 => x + sum (xs1)
    }
    
  }
  
  def product(xs:List[Int]): Int = {
    xs match {
      case List() => 1
      case x :: xs1 => x * product (xs1)
    }
  }

  def joinLists[T](xss:List[List[T]]): List[T] = {
     xss match {
      case List() => Nil
      case x :: xs1 => x ::: joinLists (xs1)
    }
  }
  
  def compress[T](xs:List[T]):List[T] = {
    xs match {
      case List() => Nil
      case x :: List() => List (x)
      case x :: xs1 => if (x == xs1.head) compress(xs1) else x :: compress(xs1)
     
    }
  }
  
}
