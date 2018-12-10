object FourInLine {


  // Declare some constants

  val NColumns = 7
  val NRows = 6

  // A player is either the red player, or the blue player

  abstract class Player(val name: String)
  case object RedPlayer extends Player("Red player")
  case object BluePlayer extends Player("Blue player")


  // A piece is either a red piece or a blue piece

  abstract class Piece(val name: String)
  case object RedPiece extends Piece("r")
  case object BluePiece extends Piece("B")

  // A column is a list of Pieces.  The head of the list represents the top of
  // the column, e.g.
  // row 6 --
  // row 5 --
  // row 4 -- RedPiece   <- head of the list
  // row 3 -- RedPiece
  // row 2 -- BluePiece
  // row 1 -- RedPiece   <- last element in the list
  // The list for this column would be List(RedPiece, RedPiece, BluePiece, RedPiece)
  // Now, to add a piece to the TOP of a column, just cons it on to the start of
  // of the column list.

  type Column = List[Piece]
  def Column(xs: (Piece)*): Column = List(xs: _*)

  // The GameState is a list of Columns

  type GameState = List[Column]
  def GameState(xs: (Column)*): GameState = List(xs: _*)

  // A newtype to help prevent off-by-one errors

  case class ColumnNum(index: Int)

  // ColumnNums are 1-based, but list indices are 0-based.  This function converts
  // a ColumnNum to a list index.

  def indexOfColumn(columnNum: ColumnNum): Int =
    columnNum match {
      case ColumnNum(index) => index - 1
    }

  //
  //   Convert a column to a string of the form "rBrrBB", or "   rrB".  The string
  //   must have length 6.  If the column is not full, then the list should be
  //   prefixed with an appropriate number of spaces
  //

  //   Convert a column to a string of the form "rBrrBB", or "   rrB".  The string
  //   must have length 6.  If the column is not full, then the list should be
  //   prefixed with an appropriate number of spaces
  //

  def showColumn(xs: Column): String = ???


  //
  //  Convert a GameState value to a string of the form:
  //  "    r        \n
  //   r   r   B   r\n
  //   B B r   B r B\n
  //   r B r r B r r\n
  //   r B B r B B r\n
  //   r B r r B r B"
  //   Useful functions:
  //     showColumn
  //       (which you already defined)
  //     transpose
  //       (transposes a list of lists,
  //       so List(List(1,2,3), List(4,5,6)) becomes List(List(1,4), List(2,5), List(3,6))
  //     mkString(sep:String) where List("a","b").mkString(" ") becomes "a b"


  def showGameState(xs : GameState): String = ???


  // Which pieces belong to which players?

  def pieceOf(player: Player): Piece = ???

  // Given a player, who is the opposing player?

  def otherPlayer(player: Player): Player = ???


  // Given a piece, what is the colour of the other player's pieces?

  def otherPiece(piece: Piece): Piece = ???


  // The initial GameState, all columns are empty.  Make sure to create the proper
  // number of columns

  def initGameState(): GameState = ???


  // Check if a column number is valid (i.e. in range)

  def isValidColumn(c: ColumnNum): Boolean = ???


  // Check if a column is full (a column can hold at most nRows of pieces)

  def isColumnFull(column: Column): Boolean = ???


  // Return a list of all the columns which are not full (used by the AI)

  def allViableColumns(game: GameState): List[ColumnNum] = ???


  // Check if the player is able to drop a piece into a column

  def canDropPiece(game: GameState, columnN: ColumnNum): Boolean = ???

  // Drop a piece into a numbered column, modifying the gamestate

  def dropPiece (game: GameState, columnN: ColumnNum, piece: Piece): GameState = ???

  // Are there four pieces of the same colour in a column?  Hint: use recursion
  // and pattern matching.

  def fourInColumn(piece: Piece, game: GameState): Boolean = ???


  // A helper function that fills up a column with pieces of a certain colour.  It
  // is used to fill up the columns with pieces of the colour that
  // fourInRow/fourInDiagonal is not looking for.  This will make those functions
  // easier to define.

  def fillBlank(piece: Piece)( column: Column): Column = ???

  // Are there four pieces of the same colour in a row?  Hint: use fillBlanks and
  // transpose to reduce the problem to fourInColumn

  def fourInRow(piece: Piece, game: GameState): Boolean = ???


  // Another helper function for fourInDiagonal.  Remove n pieces from the top of
  // a full column and add blanks (of the colour we're not looking for) to the
  // bottom to make up the difference.  This makes fourDiagonal easier to define.

  def shift(n: Int, piece: Piece, column: Column): Column = ???

  // Are there four pieces of the same colour diagonally?  Hint: define a helper
  // function using recursion, pattern matching, shift, and fourInRow.

  def fourDiagonal(piece: Piece, game: GameState): Boolean = ???

  // Are there four pieces of the same colour in a line (in any direction)

  def fourInALine(piece: Piece, game: GameState): Boolean = ???

  // Who won the game.  Returns an Option since it could be that no one has won the
  // game yet.

  def winner(game : GameState): Option[Player] = ???
  
}
