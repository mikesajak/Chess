package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Board, Move, Position}

case object Knight extends Piece {
  val symbol = "N"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    fromPos != toPos && fromPos.isInsideBoard && toPos.isInsideBoard &&
        (fromPos.rowDiff(toPos).abs == 2 && fromPos.colDiff(toPos).abs == 1 ||
            fromPos.colDiff(toPos).abs == 2 && fromPos.rowDiff(toPos).abs == 1)
  }

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    Set(fromPos.move(-1, 2),
        fromPos.move(1, 2),
        fromPos.move(-1, -2),
        fromPos.move(1, -2),
        fromPos.move(2, -1),
        fromPos.move(2, 1),
        fromPos.move(-2, -1),
        fromPos.move(-2, 1))
        .filter(_.isInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
  }
}
