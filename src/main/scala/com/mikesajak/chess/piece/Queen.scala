package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Move, Position}

case object Queen extends Piece {
  val symbol = "Q"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (fromPos == toPos || !fromPos.isInsideBoard || !toPos.isInsideBoard) false
    else {
      val colDiff = fromPos.colDiff(toPos)
      val rowDiff = fromPos.rowDiff(toPos)
      val isVerticalMove = colDiff == 0
      val isHorizontalMove = rowDiff == 0
      val isDiagonalMove = colDiff.abs == rowDiff.abs

      isVerticalMove || isHorizontalMove || isDiagonalMove
    }
  }

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    val horizMoves = for (col <- 0 to 7 if col != fromPos.col) yield
      Move(Queen, fromPos, Position(col, fromPos.row), captureAllowed = true)

    val vertMoves = for (row <- 0 to 7 if row != fromPos.row) yield
      Move(Queen, fromPos, Position(fromPos.col, row), true)

    val diagMoves = Iterable.range(1, 7).flatMap { ofs =>
      List(Move(Queen, fromPos, fromPos.move(-ofs, -ofs), true),
           Move(Queen, fromPos, fromPos.move(-ofs, ofs), true),
           Move(Queen, fromPos, fromPos.move(ofs, -ofs), true),
           Move(Queen, fromPos, fromPos.move(ofs, ofs), true))
          .filter(_.isInsideBoard)
    }

    (horizMoves ++ vertMoves ++ diagMoves).toSet
  }
}
