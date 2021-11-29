package com.mikesajak.chess.board

import com.mikesajak.chess.piece.*

enum PlayerSide(val symbol: String):
  case Black extends PlayerSide("b")
  case White extends PlayerSide("w")


object Board {

  def main(args: Array[String]): Unit = {
    val board = Board.newGame()

    println(board.printBoard)
  }

  def newGame(): Board = {
    new Board(PlayerState(startWhitePiecesPositionMap),
              PlayerState(startWhitePiecesPositionMap),
              Seq())
  }

  def allPositionsForRow(row: Int): Seq[Position] = Seq.tabulate(8)(col => Position(col, row))
  def allPositionsForRows(rows: Int*): Seq[Position] = rows.flatMap(allPositionsForRow)
  def allPositionsForCol(col: Int): Seq[Position] = Seq.tabulate(8)(row => Position(col, row))
  def allPositionsForCols(cols: Int*): Seq[Position] = cols.flatMap(allPositionsForCol)

  def allPositions: Seq[Position] =
    for (row <- 0 to 7; col <- 0 to 7) yield Position(col, row)

  /*
    Black
       0  1  2  3  4  5  6  7
    7  R  H  B  K  Q  B  H  R
    6  P  P  P  P  P  P  P  P
    5
    4
    3
    2
    1  P  P  P  P  P  P  P  P
    0  R  H  B  Q  K  B  H  R
       0  1  2  3  4  5  6  7
    White
  */
  val startWhitePiecesPositionMap: Map[Position, Piece] = {
    val whiteMainPieces = Seq(Position(0, 0) -> Rook,
                              Position(1, 0) -> Knight,
                              Position(2, 0) -> Bishop,
                              Position(3, 0) -> Queen,
                              Position(4, 0) -> King,
                              Position(5, 0) -> Bishop,
                              Position(6, 0) -> Knight,
                              Position(7, 0) -> Rook)

    val whitePawns = Seq.tabulate(8)(col => Position(col, 1) -> Pawn)

    (whiteMainPieces ++ whitePawns).toMap
  }

  val startBlackPiecesPositionMap: Map[Position, Piece] =
    startWhitePiecesPositionMap.map(p => p._1.swapSide -> p._2)

  val startPositionMap: Map[Position, (Piece, PlayerSide)] = {
    startWhitePiecesPositionMap.map(p => p._1 -> (p._2, PlayerSide.White)) ++
        startBlackPiecesPositionMap.map(p => p._1 -> (p._2, PlayerSide.Black))
  }

  val InnerRowSep: String = (0 to 7).map(_ => "----").mkString("|", "+", "|\n")
  val EdgeRowSep: String = (0 to 7).map(_ => "----").mkString("+", "+", "+\n")
}

case class Board(whitePieces: PlayerState, blackPieces: PlayerState, moves: Seq[Move]) {
  def isValid: Boolean = {
    val allPieces = whitePieces.alivePieces.toSeq ++
        blackPieces.alivePieces.toSeq.map(posPiece => posPiece._1.swapSide -> posPiece._2)

    if (allPieces.exists(posPiece => !posPiece._1.isInsideBoard)) false
    else {
      val grouped = allPieces.groupBy(posPiece => posPiece._1)
      !grouped.exists((_, set) => set.size > 1)
    }
  }

  def isCheck: Option[PlayerSide] = Option.empty // FIXME
  def isCheckMate: Option[PlayerSide] = Option.empty // FIXME

  def performMove(move: Move): Board = this // FIXME

  def isValidMove(move: Move): Boolean = {
    if (getWhitePiece(move.fromPos)
        .filter(piece => piece == move.piece)
        .exists(piece => !piece.isValidMove(move.fromPos, move.toPos, moves.isEmpty)))
      false
    else {
      if (!isOccupied(move.toPos)) true
      else move.captureAllowed
    }
  }

  def isOccupied(pos: Position): Boolean = {
    getWhitePiece(pos)
        .orElse(getBlackPiece(pos))
        .isDefined
  }

  def getPiece(row: Int, col: Int): Option[(Piece, PlayerSide)] = getPiece(Position(col, row))
  def getPiece(pos: Position): Option[(Piece, PlayerSide)] = {
    getWhitePiece(pos).map(piece => (piece, PlayerSide.White))
        .orElse(getBlackPiece(pos).map(piece => (piece, PlayerSide.Black)))
  }

  def getWhitePiece(pos: Position): Option[Piece] = whitePieces.pieceOnPos(pos)

  def getBlackPiece(pos: Position): Option[Piece] = blackPieces.pieceOnPos(pos.swapSide)

  def printBoard: String = {
    val boardRowStrings = for (rowIdx <- 7 to 0 by -1) yield {
      (0 to 7).map(colIdx => fieldSymbol(rowIdx, colIdx, this))
              .mkString("|", "|", "|\n")
    }

    boardRowStrings.mkString(Board.EdgeRowSep, Board.InnerRowSep, Board.EdgeRowSep)
  }

  private def fieldSymbol(row: Int, col: Int, board: Board) = {
    board.getPiece(row, col)
         .map(piecePos => s" ${piecePos._2.symbol}${piecePos._1.symbol} ")
         .getOrElse("    ")
  }

}
