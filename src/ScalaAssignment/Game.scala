
package ScalaAssignment
import FourInLine._
import GameTree.aiMove

object Game   {



  def main(args: Array[String]): Unit = {

    // Run tests, you specify which one here. Either all of them or individual tests
    // To run all:
    // FourInLineTests.runAllTests
    // To run one of them, first pick one and then
    // FourInLineTests.testInitGame

    // call to startGame starts a game
    def flattenN(xss:List[Any]): List[Any] = {
      xss.flatMap {case a:List[Any] => flattenN (a) case b => List(b) } 
    }
    val fullBoard = new FourInLineTests.TestBoard("fullBoard.txt") // all columns full in this board
    
    val notFullBoard = new FourInLineTests.TestBoard("redWinsRow.txt") // this board has one column full
    print(flattenN(fullBoard.board))
    print ( (fullBoard.board.zipWithIndex map ( x =>  ( x._1.count( _ == RedPiece ) ) * (3 - math.abs(3-x._2) )) ).foldLeft(0)(_ + _)   )
    print (fullBoard.board (0))
    println(allViableColumns(notFullBoard.board))
    def threeLineNum (piece: Piece, game: GameState): Int = {
      val three = Column(piece,piece,piece) 
      
      val threeInColNum = ((game map ( x=> fillBlank(piece: Piece)( x) ) map ( 
                                 x=> if ( (x.drop(3) == three) ||  //case XXXOOO
                            (x.drop(2).dropRight(1) == three)  ||  //case XXOOOX
                            (x.drop(1).dropRight(2) == three)  ||  //case XOOOXX
                            (x.dropRight(3) == three)) 1           //case OOOXXX
                            else 0) )).  foldLeft ( 0  ) ( _ + _ )   
                                                             
      val threeInRowNum = ( (game map ( x=> fillBlank(piece: Piece)( x) ) ).transpose map 
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
                                                         
    def twoLineNum (piece: Piece, game: GameState): Int = {
       val two = Column(piece,piece)
       val twoInColNum = ((game map ( x=> fillBlank(piece: Piece)( x) ) map 
                          ( x=> if ( ((x.drop(1).dropRight(3) == two) && (x.drop(4) == two)) ||       // case XOOXOO
                                   ((x.drop(3).dropRight(1) == two) && (x.dropRight(4) == two)) ) 2 // case OOXOOX
                             else if ( (x.drop(4) == two) ||        //case XXXXOO   
                            (x.drop(3).dropRight(1) == two)  ||     //case XXXOOX
                            (x.drop(2).dropRight(2) == two) ||      //case XXOOXX
                            (x.drop(1).dropRight(3) == two) ||      //case XOOXXX
                            (x.dropRight(4) == two)) 1              //case OOXXXX
                            else 0) )). foldLeft ( 0  ) ( _ + _ )   
                                                             
       val twoInRowNum = ( (game map ( x=> fillBlank(piece: Piece)( x) ) ).transpose map 
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
    print (threeLineNum (RedPiece, fullBoard.board ))
    print (twoLineNum (RedPiece, fullBoard.board ))
    FourInLineTests.runAllTests
    startGame
  }


  // A map (similar to a dictionary in Python, or a HashMap in Java) that maps
  // Players to the  functions that get moves for those players.  This will
  // allow us to use the same code for human vs. human matches as for computer
  // vs. human and computer vs. computer.

  type MoveGetterMap = Map[Player, GameState => ColumnNum]

  def MoveGetterMap(xs: (Player, GameState => ColumnNum)*) = Map(xs: _*)

  // How many moves should the AI look ahead.  Higher numbers mean a smarter AI,
  // but it takes much longer to evaluate the game tree.

  val aiDepth = 4

  


  // UI routines

  def startGame() = {
    println("Welcome to four-in-line")
    val redPlayer = getMoveGetter(RedPlayer)
    val bluePlayer = getMoveGetter(BluePlayer)
    val moveGetter = MoveGetterMap(RedPlayer -> redPlayer, BluePlayer -> bluePlayer)
    turn(moveGetter, RedPlayer, initGameState)
  }

  // Execute a single turn

  def turn(moveGetter: MoveGetterMap, player: Player, game: GameState): Unit = {

    val win = winner(game)

    if (win != None) {
      win match {
        case Some(p) => {
          drawBoard(game)
          println(p.name + " wins!")
        }
        case None => {}
        
      }
    } else if (allViableColumns(game).isEmpty) {
      drawBoard(game)
      println("It's a draw")
    } else {
      val c = (moveGetter(player))(game)
      val gameP = dropPiece(game, c, pieceOf(player))
      drawBoard(game)
      turn(moveGetter, otherPlayer(player), gameP)
    }

  }

  // gets a function that gets the next move for a particular player.
  // Depending on whether the player is human or computer, it will be
  // getHumanMove player, or getComputerMove player

  def getMoveGetter(player: Player): (GameState => ColumnNum) = {

    println("Is " + player.name + " to be human or computer? ")

    val ln = scala.io.StdIn.readLine().trim

    if (ln.equals("c")) {
      aiMove(aiDepth, player)
      
    } else if (ln.equals("h")) {
      getHumanMove(player)
    } else {
      println("Either \"human\" or \"computer\"")
      getMoveGetter(player)
    }
  }


  // Read a valid move

  def getHumanMove(player: Player)(game: GameState): ColumnNum = {
    drawBoard(game)
    println(player.name + "'s turn. Enter column number: ")



    def getValidMove(): ColumnNum = {
      val c = getMove()
      if (!canDropPiece(game, c)) {
        println("That column is full, try again.")
        getValidMove();
      } else
          c
    }

    def getMove(): ColumnNum = {
      val ln = scala.io.StdIn.readLine().trim
      getColumn(ln) match {
        case Some(c) => {
          if (isValidColumn(c)) c else {
            println("No such column, try again.")
            println("Enter column number: ")
            getMove()
          }
        }
        case None => {
          println("That wasn't a number. Enter column number: ")
          getMove()
        }
      }
    }

    getValidMove    

  }

  // Parse a column number from a string

  def getColumn(str: String): Option[ColumnNum] = {
    val c = str.trim
    try {
      Some(ColumnNum(c.toInt))
    } catch {
      case e: Exception => None
    }
  }


  //Draw the game board

  def drawBoard(gameState: GameState): Unit = {
    val board = showGameState(gameState)
    println("  1 2 3 4 5 6 7")
    val output = for ( (r, l) <- board.split("\n").zip("654321".toList) ) yield {
      l.toString().concat(" ").concat(r)
    }
    println(output.mkString("\n"))

  }
}


