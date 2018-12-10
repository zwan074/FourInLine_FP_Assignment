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
    if (fourInALine(pieceOf(player), game))
      100
    else if (fourInALine(pieceOf(otherPlayer(player)), game))
      -100
    else
      0
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

