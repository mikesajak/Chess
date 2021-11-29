package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Move, Position}

object Rook extends Piece {
  val symbol = "R"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean =
    fromPos != toPos && (fromPos.colDiff(toPos) == 0 || fromPos.rowDiff(toPos) == 0)

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    ((0 to 7).view
             .filter(row => row != fromPos.row)
             .map(row => Position(fromPos.col, row)) ++
        (0 to 7).view
                .filter(col => col != fromPos.col)
                .map(col => Position(col, fromPos.row)))
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
        .toSet
  }
}
