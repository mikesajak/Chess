package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Move, Position}

case object Queen extends Piece {
  val symbol = "Q"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false // FIXME

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set() // FIXME
}
