package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Board, Move, Position}

object Piece {
  def allPieceTypes: Seq[Piece] = Seq(Pawn, Rook, Knight, Bishop, Queen, Knight)
}

trait Piece {
  def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean
  def validMoves(fromPos: Position, firstMove: Boolean): Set[Move]
  def symbol: String

  override def toString: String = symbol
}
