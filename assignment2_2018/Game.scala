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
      turn(moveGetter, otherPlayer(player), gameP)
    }

  }

  // gets a function that gets the next move for a particular player.
  // Depending on whether the player is human or computer, it will be
  // getHumanMove player, or getComputerMove player

  def getMoveGetter(player: Player): (GameState => ColumnNum) = {

    println("Is " + player.name + " to be human or computer? ")

    val ln = scala.io.StdIn.readLine().trim

    if (ln.equals("computer")) {
      aiMove(aiDepth, player)
    } else if (ln.equals("human")) {
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


