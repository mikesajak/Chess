package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Board, Move, Position}

case object Bishop extends Piece {
  val symbol = "B"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (!Board.posInsideBoard(fromPos) || !Board.posInsideBoard(toPos) || fromPos == toPos) false
    else fromPos.rowDiff(toPos) == fromPos.colDiff(toPos)
  }

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    (1 to 7).flatMap(ofs =>
                       Seq(fromPos.move(ofs, ofs), fromPos.move(-ofs, ofs), fromPos.move(ofs, -ofs), fromPos.move(-ofs,
                                                                                                                  -ofs)))
            .filter(Board.posInsideBoard)
            .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
            .toSet
  }
}
