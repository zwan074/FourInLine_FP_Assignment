package ScalaAssignment

import FourInLine._

object GameTree {

  case class GameTree(val gameState: GameState, val move: List[Move])

  case class Move(val move: ColumnNum, val tree: GameTree)


  // Recursively build the game tree using allViableColumns to get all possible
  // moves (introduce depth as the function is not lazy).  Note that the tree bottoms out once the game is won

  def gameTree(player: Player, depth: Int, game: GameState): GameTree = {

    def subGameTree(c: ColumnNum): Move = {
      Move(c, gameTree(otherPlayer(player),  depth-1, dropPiece(game, c, pieceOf(player))))
    }

    val w = winner(game)
    w match {
      case Some(p) => GameTree(game, List())
      case _ => {
        if (depth == 0)
          GameTree(game, List())
        else
          GameTree(game, allViableColumns(game).map(subGameTree))
      }
    }
  }



  //Estimate the value of a position for a player. This implementation only
  //assigns scores based on whether or not the player has won the game.  This is
  //the simplest possible way of doing it, but it results in an
  //overly-pessimistic AI.
  //
  //The "cleverness" of the AI is determined by the sophistication of the
  //estimate function.
  //Some ideas for making the AI smarter:
  //1) A win on the next turn should be worth more than a win multiple turns
  //later.  Conversely, a loss on the next turn is worse than a loss several
  //turns later.
  //2) Some columns have more strategic value than others.  For example, placing
  //pieces in the centre columns gives you more options.
  //3) It's a good idea to clump your pieces together so there are more ways you
  //could make four in a line.


  def estimate(player: Player, game: GameState): Int = {
    //help function to compute total number of pieces in board
    def flattenN(xss:List[Any]): List[Any] = {
      xss.flatMap {case a:List[Any] => flattenN (a) case b => List(b) } 
    }
    
    //compute number of 3 same pieces in a line 
    def threeLineNum (piece: Piece, game: GameState): Int = {
       val three = Column(piece,piece,piece) 
      
       val threeInColNum = ((game map ( fillBlank(piece)_ ) map ( 
                                 x=> if ( (x.drop(3) == three) ||  //case XXXOOO
                            (x.drop(2).dropRight(1) == three)  ||  //case XXOOOX
                            (x.drop(1).dropRight(2) == three)  ||  //case XOOOXX
                            (x.dropRight(3) == three)) 1           //case OOOXXX
                            else 0) )).  foldLeft ( 0  ) ( _ + _ )   
                                                             
       val threeInRowNum = ( (game map ( fillBlank(piece)_ ) ).transpose map 
                ( x => 
                 if ((x.drop(4) == three) && (x.dropRight(4) == three)) 2 //case OOOXOOO
                 else if ((x.dropRight(4) == three) ||//case OOOXXXX
                (x.drop(1).dropRight(3) == three)  || //case XOOOXXX
                (x.drop(2).dropRight(2) == three) ||  //case XXOOOXX
                (x.drop(3).dropRight(1) == three) ||  //case XXXOOOX
                (x.drop(4) == three)) 1               //case XXXXOOO
                 else 0)). foldLeft ( 0  ) ( _ + _  )    
                                                             
        threeInColNum + threeInRowNum                                                     
    }                                                       
    //compute number of 2 pieces in a line                                                     
    def twoLineNum (piece: Piece, game: GameState): Int = {
       val two = Column(piece,piece)
       val twoInColNum = ((game map ( fillBlank(piece)_) map 
                          ( x=> if ( ((x.drop(1).dropRight(3) == two) && (x.drop(4) == two)) ||       // case XOOXOO
                                   ((x.drop(3).dropRight(1) == two) && (x.dropRight(4) == two)) ) 2 // case OOXOOX
                             else if ( (x.drop(4) == two) ||        //case XXXXOO   
                            (x.drop(3).dropRight(1) == two)  ||     //case XXXOOX
                            (x.drop(2).dropRight(2) == two) ||      //case XXOOXX
                            (x.drop(1).dropRight(3) == two) ||      //case XOOXXX
                            (x.dropRight(4) == two)) 1              //case OOXXXX
                            else 0) )). foldLeft ( 0  ) ( _ + _ )   
                                                             
       val twoInRowNum = ( (game map ( fillBlank(piece)_ ) ).transpose map 
                ( x => if ( ((x.dropRight(5) == two) && (x.drop(3).dropRight(2) == two) ) ||  //case OOXOOXX
                 ((x.drop(1).dropRight(4) == two) && (x.drop(4).dropRight(1) == two))||  //case XOOXOOX
                 ((x.drop(2).dropRight(3) == two) && (x.drop(5) == two)) ) 2              //case XXOOXOO 
                 else if ( (x.dropRight(5) == two) ||  //case OOXXXXX
                  (x.drop(1).dropRight(4) == two)  ||    //case XOOXXXX
                  (x.drop(2).dropRight(3) == two) ||     //case XXOOXXX
                  (x.drop(3).dropRight(2) == two) ||     //case XXXOOXX
                  (x.drop(4).dropRight(1) == two) ||     //case XXXXOOX
                  (x.drop(5) == two) ) 1                  //case XXXXXOO
                 else 0)). foldLeft ( 0  ) ( _ + _  )    
                                                             
        twoInColNum + twoInRowNum                                                     
      } 
    
    //compute score for amount of piece close to centre column
    val controlCenter = (game.zipWithIndex map ( x =>   x._1.count( _ == pieceOf(player) )  * (3 - math.abs(3-x._2) ) ) ).foldLeft(0)(_ + _)  
    
    
    //1) Center column has higher score than side columns. Number of 3 in lines and 2 in lines good for player and bad to opponent
    if (winner(game) == None ) 
    3 * threeLineNum ( pieceOf(player), game) +  2 * twoLineNum ( pieceOf(player), game) - 3 * threeLineNum (  pieceOf(otherPlayer(player)), game) - 2* twoLineNum (  pieceOf(otherPlayer(player)), game) + controlCenter
    //2) if player win earlier could have higher score than win later, it works reversely to opponent   .
    else if (fourInALine(pieceOf(player), game)) 10000 - flattenN (game).length
    else if (fourInALine(pieceOf(otherPlayer(player)), game)) -10000 + flattenN (game).length
    else 0

   
    
  }



  // Find the move that maximises the minimum utility to a player, assuming it is
  // that player's turn to move.

  def maxmini(player: Player, tree: GameTree): ColumnNum = {
    tree match {
      case GameTree(x, List()) => throw new Exception("The AI was asked to make a move, but there are no moves possible.  This cannot happen")
      case GameTree(x, moves) => moves.maxBy(m => minimaxP(player, m.tree)).move
    }
  }


  // Maximise the minimum utility of player making a move.  Do this when it is the
  // player's turn to find the least-bad move, assuming the opponent will play
  // perfectly.


  def maxminiP(player: Player, tree: GameTree): Int = {
    tree match {
      case GameTree(x, List()) => estimate(player, x)
      case GameTree(x, moves) => moves.map(m => minimaxP(player, m.tree)).max
    }
  }

  // Minimise the maximum utility of player making a move.  Do this when it is the
  // opponent's turn, to simulate the opponent choosing the move that results in
  // the least utility for the player.


  def minimaxP(player: Player, tree: GameTree): Int = {
    tree match {
      case GameTree(x,List()) => estimate(player,x)
      case GameTree(x,moves) => moves.map(m => maxminiP(player, m.tree)).min
    }
  }


  // Determine the best move for a player

  def aiMove(lookahead: Int, player: Player): (GameState => ColumnNum)  = {
    x => {
      maxmini(player,gameTree(player, lookahead, x))
      
    }
  }

}

