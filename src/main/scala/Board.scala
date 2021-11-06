package com.mikesajak.chess

enum PlayerSide(val symbol: String):
  case Black extends PlayerSide("b")
  case White extends PlayerSide("w")


object Board {

  def main(args: Array[String]): Unit = {
    val board = new Board(PlayerPieces.createPlayerPieces(),
                          PlayerPieces.createPlayerPieces(), Seq())

    println(board.printBoard)
  }

  def newGame() = new Board(PlayerPieces.createPlayerPieces(), PlayerPieces.createPlayerPieces(), Seq())

}

case class Board(whitePieces: PlayerPieces, blackPieces: PlayerPieces, moves: Seq[Move]) {
//  def isValid: Boolean
//  def isCheck: Boolean
//  def isCheckMate: Boolean

//  def playerAPieces: PlayerPieces
//  def playerBPieces: PlayerPieces

//  def isValidMove(move: Move): Boolean
//  def performMove(move: Move): Board

  def isValidMove(move: Move): Boolean = {
    if (getWhitePiece(move.fromPos)
        .filter(piece => piece == move.piece)
        .exists(piece => piece.isValidMove(move.fromPos, move.toPos, moves.isEmpty)))
      false
    else {
      if (!isOccupied(move.toPos)) true
      else {
        false // FIXME
      }

    }
  }

  def isOccupied(pos: Position): Boolean = {
    getWhitePiece(pos)
        .orElse(getBlackPiece(pos))
        .isDefined
  }

  def getPiece(row: Int, col: Int): Option[(Piece, PlayerSide)] = getPiece(Position(col, row))
  def getPiece(pos: Position): Option[(Piece, PlayerSide)] = {
    val whitePiece = getWhitePiece(pos)
    val blackPiece = getBlackPiece(pos)
    whitePiece.map(piece => (piece, PlayerSide.White))
        .orElse(blackPiece.map(piece => (piece, PlayerSide.Black)))
  }

  def getWhitePiece(pos: Position): Option[Piece] = {
    whitePieces.pieceOnPos(mapToSide(PlayerSide.White, pos))
  }

  def getBlackPiece(pos: Position): Option[Piece] = {
    blackPieces.pieceOnPos(mapToSide(PlayerSide.Black, pos))
  }

  private def mapToSide(side: PlayerSide, pos: Position): Position = side match {
    case PlayerSide.White => pos
    case PlayerSide.Black => Position(7 - pos.col, 7 - pos.row)
  }

  def printBoard: String = {
    val boardRowStrings = for (rowIdx <- 7 to 0 by -1) yield {
      (0 to 7).map(colIdx => fieldSymbol(rowIdx, colIdx, this))
              .mkString("|", "|", "|\n")
    }

    boardRowStrings.mkString(EdgeRowSep, InnerRowSep, EdgeRowSep)
  }

  private val InnerRowSep  = (0 to 7).map(_ => "----").mkString("|", "+", "|\n")
  private val EdgeRowSep = (0 to 7).map(_ => "----").mkString("+", "+", "+\n")


  private def fieldSymbol(row: Int, col: Int, board: Board) = {
    board.getPiece(row, col)
         .map(piecePos => s" ${piecePos._2.symbol}${piecePos._1.pieceType.symbol} ")
         .getOrElse("    ")
  }

}
