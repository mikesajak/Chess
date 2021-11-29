package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Board, Move, Position}

case object King extends Piece {
  val symbol = "K"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean =
    fromPos.isInsideBoard && toPos.isInsideBoard
        && fromPos != toPos
        && (fromPos.colDiff(toPos).abs <= 1 && fromPos.rowDiff(toPos).abs <= 1)

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] =
    Set(fromPos.move(-1, 1), fromPos.move(0, 1), fromPos.move(1, 1),
        fromPos.move(-1, 0), fromPos.move(1, 0),
        fromPos.move(-1, -1), fromPos.move(0, -1), fromPos.move(1, -1))
        .filter(_.isInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
}
