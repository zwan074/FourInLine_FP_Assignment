import FourInLine._
import scala.io.Source


object FourInLineTests {


  // all tests together
  def runAllTests(): Unit = {

    testInitGame
    testShowColumn
    testShowGameState
    testPieceOf
    testOtherPlayer
    testOtherPiece
    testIsValidColumn
    testIsColumnFull
    testViableColumns
    testCanDropPiece
    testDropPiece
    testFourInCol
    testFourInRow
    testFourDiagonal
    testFourInALine
    testWinner
  }

  class TestBoard(fileName: String) {

    val lines = Source.fromURL(getClass.getResource("/" + fileName)).getLines.toList
    val columns = lines.transpose.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val withoutBlanks = columns.map(x => x.filter(_ != ' '))

    if (columns.length != 7 || !columns.forall(c => c.length == 6)) {
      throw new RuntimeException("Board file is not valid: " + fileName)
    }

    def toPiece(c: Char): Piece = {
      if (c == 'r')
        RedPiece
      else if (c == 'B')
        BluePiece
      else
        throw new RuntimeException("Invalid piece: '" + c + "' in board file: " + fileName)
    }


    val boardColumns = withoutBlanks.map(x => x.map(toPiece))

    def display = lines.mkString("\n")

    def board = boardColumns
  }





  def testInitGame() = {
    assert(initGameState().size == 7)
    assert(initGameState.map(x => x.size).sum == 0)
    println("initGame tested")

  }

  def testShowColumn() = {
    val c1 = Column(RedPiece, BluePiece, RedPiece, RedPiece, RedPiece)
    assert(showColumn(c1) == " rBrrr")
    val c2 = Column(RedPiece, RedPiece, BluePiece, RedPiece, RedPiece, BluePiece, RedPiece, RedPiece)
    assert(!(showColumn(c2) == " rBrrr"))
    assert(showColumn(c2).size > 7)
    println("showColumn tested")

  }


  def testShowGameState() = {
    val g = new TestBoard("redWinsRow.txt")
    assert(showGameState(g.board) == g.display)
    println("showGameState tested")

  }

  def testPieceOf() = {
    assert(pieceOf(RedPlayer) == RedPiece)
    assert(pieceOf(BluePlayer) == BluePiece)
    println("pieceOf tested")
  }


  def testOtherPlayer() = {
    assert(otherPlayer(RedPlayer) == BluePlayer)
    assert(otherPlayer(BluePlayer) == RedPlayer)
    println("otherPlayer tested")

  }

  def testOtherPiece() = {
    assert(otherPiece(RedPiece) == BluePiece)
    assert(otherPiece(BluePiece) == RedPiece)
    println("otherPiece tested")

  }

  def testIsValidColumn() = {
    assert(!isValidColumn(ColumnNum(0)))
    assert(!isValidColumn(ColumnNum(8)))
    assert(isValidColumn(ColumnNum(1)))
    assert(isValidColumn(ColumnNum(7)))
    println("isValidColumn tested")

  }


  def testIsColumnFull() = {
    val fullColumn = Column(RedPiece, RedPiece, BluePiece, BluePiece, RedPiece, RedPiece)
    val notFullColumn = Column()
    val notFullColumn2 = Column(RedPiece)
    assert(isColumnFull(fullColumn))
    assert(!isColumnFull(notFullColumn))
    assert(!isColumnFull(notFullColumn2))
    println("isColumnFull tested")

  }


  def testViableColumns() = {
    val fullBoard = new TestBoard("fullBoard.txt") // all columns full in this board
    assert(allViableColumns(fullBoard.board).isEmpty)
    val notFullBoard = new TestBoard("redWinsRow.txt") // this board has one column full
    assert(allViableColumns(notFullBoard.board).length == 6)
    println("allViableColumns tested")

  }

  def testCanDropPiece() = {
    val fullBoard = new TestBoard("fullBoard.txt") // all columns full in this board
    assert(!canDropPiece(fullBoard.board, ColumnNum(1)))
    val notFullBoard = new TestBoard("redWinsRow.txt") // this board has one column full
    assert(!canDropPiece(notFullBoard.board, ColumnNum(3)))
    assert(canDropPiece(notFullBoard.board, ColumnNum(1)))
    println("canDropPiece tested")

  }

  def testDropPiece() = {
    val beforeDrop = new TestBoard("redWinsRow.txt") // before drop piece
    val afterDrop = new TestBoard("dropPieceRed.txt") // after drop
    assert(dropPiece(beforeDrop.board, ColumnNum(2), RedPiece) == afterDrop.board)
    println("dropPiece tested")

  }

  def testFourInCol() = {
    val fourInCol = new TestBoard("fourInCol.txt")
    val noFour = new TestBoard("noFourInLine.txt")
    assert(fourInColumn(BluePiece, fourInCol.board))
    assert(!fourInColumn(BluePiece, noFour.board))
    assert(!fourInColumn(RedPiece, fourInCol.board))
    println("fourInColumn tested")

  }

  def testFourInRow() = {
    val fourInRowBoard = new TestBoard("fourInRow.txt")
    val noFour = new TestBoard("noFourInLine.txt")
    assert(fourInRow(BluePiece, fourInRowBoard.board))
    assert(!fourInRow(BluePiece, noFour.board))
    assert(!fourInRow(RedPiece, fourInRowBoard.board))
    println("fourInRow tested")

  }


  def testFourDiagonal() = {
    val fourDiagonalBoard = new TestBoard("fourDiagonal.txt")
    val fd2 = new TestBoard("fourDiagonal2.txt")
    val fd3 = new TestBoard("fourDiagonal3.txt")

    val noFour = new TestBoard("noFourInLine.txt")
    assert(fourDiagonal(BluePiece, fourDiagonalBoard.board))
    assert(!fourDiagonal(BluePiece, noFour.board))
    assert(!fourDiagonal(RedPiece, fourDiagonalBoard.board))
    assert(fourDiagonal(RedPiece, fd2.board))
    assert(fourDiagonal(BluePiece, fd3.board))
    println("fourDiagonal tested")

  }

  def testFourInALine() = {
    val fd = new TestBoard("fourDiagonal.txt")
    val fr = new TestBoard("fourInRow.txt")
    val fc = new TestBoard("fourInCol.txt")
    assert(fourInALine(BluePiece, fd.board))
    assert(fourInALine(BluePiece, fr.board))
    assert(fourInALine(BluePiece, fc.board))
    println("fourInALine tested")

  }

  def testWinner() = {
    val fourDiagonalBoard = new TestBoard("fourDiagonal.txt")
    assert(winner(fourDiagonalBoard.board) == Some(BluePlayer))
    println("winner tested")

  }


}
