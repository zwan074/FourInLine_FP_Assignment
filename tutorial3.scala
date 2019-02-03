
package Tutorial

object tutorial3 {
  
 
  def main(args: Array[String]) = {
  
    print (applyEach(List((x=>x*x),(x=>x+1),(x=>List.fill(3)(x))),5) +"\n" )
    print (twice((x:Int) => x+5, 1) + "\n")
    print (iter((x:Int) => x+5, 5, 1) + "\n")
    print (liftTwice((x:Int) => x+5) (1) + "\n")
    print (liftIter((x:Int) => x+5 , 5) (1) + "\n")
    
    val e1 = BinOp("*", BinOp("/", Number(1), Number(2)),UnOp("-" ,BinOp("+", Var("x"), Number(1)) ))
    val e2 = BinOp("+", BinOp("/", Var("x"), Number(2)), BinOp("/", Number(1.5), Var("y")))
    val e3 = UnOp("-", e1)
    
    print (exprSize(e1) + "\n")
    print (exprSize(e2) + "\n")
    print (exprSize(e3) + "\n")
    print (exprToString(e1) + "\n")
    print (exprToString(e2) + "\n")
    print (exprToString(e3) + "\n")
    
    val test1:Hand = List (  Card (Heart,Ace),Card (Heart,Eight),Card (Heart,Seven),  
                              Card (Heart,Jack), Card (Heart,Six), Card (Heart,Five), 
                              Card (Heart,King), Card (Diamond,Six) ,Card (Diamond,Five) ,
                              Card (Spade,Eight) ,Card (Spade,Seven) ,Card (Spade,Six), Card (Spade,Five)) 
                              
    val test2:Hand = List (  Card (Heart,Ace),Card (Heart,Eight),Card (Heart,Seven),  
                              Card (Heart,Jack), Card (Heart,Six), Card (Heart,Four), 
                              Card (Club,King), Card (Diamond,Jack) ,Card (Diamond,Ace) ,
                              Card (Spade,Eight) ,Card (Spade,Seven) ,Card (Spade,Six), Card (Spade,Five))  
                              
    val test3:Hand = List (  Card (Heart,Ace),Card (Heart,King),Card (Heart,Queen),  
                              Card (Club,Seven), Card (Club,Six), Card (Club,Four), 
                              Card (Diamond,King), Card (Diamond,Queen) ,Card (Diamond,Ace) ,
                              Card (Spade,Eight) ,Card (Spade,Seven) ,Card (Spade,Six), Card (Spade,Five)) 
                              
    val test4:Hand = List (  Card (Heart,Ace),Card (Heart,King),Card (Heart,Queen),  
                              Card (Club,Ace), Card (Club,Six), Card (Club,Four), 
                              Card (Diamond,King), Card (Diamond,Queen) ,Card (Diamond,Ace) ,
                              Card (Spade,Eight) ,Card (Spade,Seven) ,Card (Spade,Six), Card (Spade,Five))                            
                              
    val test5:Hand = List (  Card (Heart,Ace),Card (Heart,Five),Card (Heart,Six),  
                              Card (Club,Ace), Card (Heart,Six), Card (Heart,Four), 
                              Card (Diamond,King), Card (Diamond,Queen) ,Card (Diamond,Ace) ,
                              Card (Spade,Ace) ,Card (Club,Four) ,Card (Spade,Six), Card (Diamond,Five))  
                              
    val test6:Hand = List (  Card (Heart,Ace),Card (Heart,King),Card (Heart,Queen),  
                              Card (Club,Ace), Card (Club,Six), Card (Club,Four), 
                              Card (Diamond,King), Card (Diamond,Queen) ,Card (Diamond,Ace) ,
                              Card (Spade,Ace) ,Card (Spade,King) ,Card (Spade,Six), Card (Spade,Five)) 
                              
    val test7:Hand = List (  Card (Heart,Ace),Card (Heart,Five),Card (Heart,Four),  
                              Card (Club,Ace), Card (Club,Six), Card (Club,Four), 
                              Card (Diamond,Six), Card (Diamond,Five) ,Card (Diamond,Four) ,
                              Card (Spade,Eight) ,Card (Spade,Seven) ,Card (Spade,Six), Card (Spade,Five))   
                              
    print ("High Class Point:"+ HCP(test1) + " SevenInOneSuit " + SevenInOne (test1) + " Open Bid :" + openingBid (test1) + "\n")
    print ("High Class Point:"+ HCP(test2) + " 5+ Hearts " + Hearts (test2) + " Open Bid :" + openingBid (test2) + "\n")
    print ("High Class Point:"+ HCP(test3) + " isBalancedHands " + BalancedHands (test3) + " Open Bid :" + openingBid (test3) + "\n")
    print ("High Class Point:"+ HCP(test4) + " isBalancedHands " + BalancedHands (test4) + " Open Bid :" + openingBid (test4) + "\n")
    print ("High Class Point:"+ HCP(test5) + " isBalancedHands " + BalancedHands (test5) + " Open Bid :" + openingBid (test5) + "\n")
    print ("High Class Point:"+ HCP(test6) + " isBalancedHands " + BalancedHands (test6) + " Open Bid :" + openingBid (test6) + "\n")
    print ("High Class Point:"+ HCP(test7) + " isBalancedHands " + BalancedHands (test7) + " Open Bid :" + openingBid (test7) + "\n")
    
  }

  def applyEach(fnList:List[(Int=>Any)], x:Int): List[Any] = fnList match{
    
    case Nil => Nil
    case f::rest => f(x) :: applyEach (rest,x)
    
  }
  
  def twice[T](fn:(T=>T), x:T): T =
  {
    fn(fn(x))
  }
  
  def iter[T](fn:(T=>T), n:Int, x:T): T = n match
  {
    case 0 => x
    case _ => fn (iter ( fn,n-1,x ) )
  }
  
  def liftTwice[T](fn:(T=>T)): (T => T) =
  {
    fn compose fn
  }
  
  def liftIter[T](fn:(T=>T), n:Int): (T => T) = n match
  {
    case 0 => (x:T) => x
    case _ => fn compose liftIter( fn,n-1 ) 
              
  }
  
  sealed abstract class Expr
  case class Var(name:String) extends Expr
  case class Number(num:Double) extends Expr
  case class UnOp(operator:String, arg:Expr) extends Expr
  case class BinOp(operator:String, left:Expr, right:Expr) extends Expr
  
  def exprSize(e: Expr):Int = e match
  {
    case Var(_) => 1
    case Number(_) => 1
    case UnOp(op,e) => 1 + exprSize(e)
    case BinOp (op,e1,e2) => 1 + exprSize(e1) + exprSize(e2)
  }
  
  def exprToString(e: Expr):String = e match
  { 
    case Var(x) => x
    case Number(x) => x.toString
    case UnOp(op,e) =>"(" + op.toString()  + exprToString(e) + ")"    
    case BinOp (op,e1,e2) => "("+exprToString(e1) + op.toString() + exprToString(e2)+")"
  }
 
  sealed abstract class Suit
  case object Club extends Suit
  case object Diamond extends Suit
  case object Heart extends Suit
  case object Spade extends Suit

  sealed abstract class FaceValue
  case object Ace extends FaceValue
  case object King extends FaceValue
  case object Queen extends FaceValue
  case object Jack extends FaceValue
  case object Ten extends FaceValue
  case object Nine extends FaceValue
  case object Eight extends FaceValue
  case object Seven extends FaceValue
  case object Six extends FaceValue
  case object Five extends FaceValue
  case object Four extends FaceValue
  case object Three extends FaceValue
  case object Deuce extends FaceValue

  sealed case class Card(suit:Suit, value:FaceValue) 

  type Hand = List[Card]
  
  sealed abstract class Trumps
  case object TClub extends Trumps
  case object TDiamond extends Trumps
  case object THeart extends Trumps
  case object TSpade extends Trumps
  case object NoTrump extends Trumps
  
  sealed abstract class Bid
  case object Pass extends Bid
  case class OpBid(level: Int, trumpSuit: Trumps) extends Bid

  
  def openingBid(myHand:Hand): Bid = 
  {
    
    val HighClassPoint = HCP(myHand)
    val SevenInOneSuit = SevenInOne (myHand)
    val isSpades = Spades(myHand)
    val isHearts = Hearts (myHand)
    val isDiamonds = Diamonds (myHand)
    val isBalancedHands = BalancedHands (myHand)
    
    
    if ( (HighClassPoint >= 7 && HighClassPoint <=11 ) && 
         ((SevenInOneSuit == "Diamond") || (SevenInOneSuit == "Heart") 
          || (SevenInOneSuit == "Spade")  || (SevenInOneSuit == "Club")) ) {
      
      if (SevenInOneSuit == "Diamond") OpBid(3, TDiamond)
      else if (SevenInOneSuit == "Heart") OpBid(3, THeart)
      else if (SevenInOneSuit == "Spade") OpBid(3, TSpade)
      else OpBid(3, TClub)
    }  
    
    else if ( (HighClassPoint >= 12 && HighClassPoint <=15 ) ||
             ((HighClassPoint >= 16 && HighClassPoint <=18 ) && (isBalancedHands == false ))){
      if ( isSpades  &&  isHearts) OpBid(1, TSpade)
      else if (isSpades ) OpBid(1, TSpade)
      else if (isHearts ) OpBid(1, THeart)
      else if (isDiamonds) OpBid(1, TDiamond)
      else OpBid(1, TClub)
      
    }
    
    else if ((HighClassPoint >= 16 && HighClassPoint <=18 ) && (isBalancedHands == true)) OpBid(1, NoTrump)
        
    else if ((HighClassPoint >= 20 && HighClassPoint <=22 ) && (isBalancedHands == true)) OpBid(2, NoTrump)
    
    else if (HighClassPoint >= 19 && HighClassPoint <=21 ) OpBid(2, TClub)
    
    else if (HighClassPoint >= 22 ) OpBid(2, TDiamond)
    
    else Pass
    
   
  }

  def HCP (myHand: Hand) : Int = myHand match
  {
    case Nil => 0
    case head :: tail => 
      if (head.value == Ace ) 4 + HCP (tail) 
      else if (head.value == King ) 3 + HCP (tail) 
      else if (head.value == Queen) 2 + HCP (tail) 
      else if (head.value == Jack ) 1 + HCP (tail)         
      else 0 + HCP (tail) 
    
  }
  
  def Spades (myHand: Hand) : Boolean = {
    
     myHand.count ( _.suit == Spade  ) > 5
    
  }
  
  def Hearts (myHand: Hand) : Boolean = {
    
    myHand.count ( _.suit == Heart  ) > 5
    
  }
  
  def Diamonds (myHand: Hand) : Boolean = {
    
      myHand.count ( _.suit == Diamond  ) > 4
        
  }
  
  def SevenInOne (myHand: Hand) : String = {
    
  
    if (myHand.count ( _.suit == Diamond  ) >= 7) "Diamond"
    else if (myHand.count ( _.suit == Heart  ) >= 7) "Heart"
    else if (myHand.count ( _.suit == Spade  ) >= 7) "Spade"
    else if (myHand.count ( _.suit == Club  ) >= 7) "Club"
    else "None"  
    
    
  }
  
  def BalancedHands (myHand: Hand) : Boolean = {
    
    if ( ((myHand.count ( _.suit == Diamond  ) >= 2) && (myHand.count ( _.suit == Heart  ) >= 3) 
        && (myHand.count ( _.suit == Spade  ) >= 3) && (myHand.count ( _.suit == Club  ) >= 3)) 
      || ((myHand.count ( _.suit == Diamond  ) >= 3) && (myHand.count ( _.suit == Heart  ) >= 2) 
        && (myHand.count ( _.suit == Spade  ) >= 3) && (myHand.count ( _.suit == Club  ) >= 3)) 
    || ((myHand.count ( _.suit == Diamond  ) >= 3) && (myHand.count ( _.suit == Heart  ) >= 3) 
        && (myHand.count ( _.suit == Spade  ) >= 2) && (myHand.count ( _.suit == Club  ) >= 3)) 
    || ((myHand.count ( _.suit == Diamond  ) >= 3) && (myHand.count ( _.suit == Heart  ) >= 3) 
        && (myHand.count ( _.suit == Spade  ) >= 3) && (myHand.count ( _.suit == Club  ) >= 2)) ) true  
    else false
    
  }
}