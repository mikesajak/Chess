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

case object Pawn extends Piece {
  import Piece.*

  val symbol = "P"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (fromPos.col < 0 || fromPos.col > 7
        || fromPos.row < 0 || fromPos.row > 7) false
    else if (firstMove && fromPos.row != 1) false
    else {
      Board.posInsideBoard(toPos)
          && (isRegularMove(fromPos, toPos, firstMove)
          || isCaptureMove(fromPos, toPos))
    }
  }

  private def isRegularMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    val allowedForwardMove = if (firstMove) 2 else 1
    toPos.row - fromPos.row <= allowedForwardMove
        && fromPos.col == toPos.col
  }

  private def isCaptureMove(fromPos: Position, toPos: Position): Boolean =
    toPos.row - fromPos.row == 1 &&
        math.abs(fromPos.col - toPos.col) == 1

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    val regularTargetPos = Set(fromPos.move(0, 1))
    // TODO: maybe mark this move - to enable detecting of "en passant" capture
    val regularFirstMoveTargetPos = if (firstMove) Set(fromPos.move(0, 2)) else Set()
    val captureTargetPositions = Set(fromPos.move(-1, 1),
                                     fromPos.move(1, 1))

    val regularMoves = (regularTargetPos ++ regularFirstMoveTargetPos).view
        .filter(Board.posInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = false))

    val captureMoves = captureTargetPositions.view
        .filter(Board.posInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true, onlyCapture = true))

    (regularMoves ++ captureMoves).toSet
  }

}

case object Knight extends Piece { // horse
  val symbol = "N"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    fromPos != toPos && Board.posInsideBoard(fromPos) && Board.posInsideBoard(toPos) &&
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
        .filter(Board.posInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
  }
}

case object Bishop extends Piece { // Laufer
  val symbol = "B"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (!Board.posInsideBoard(fromPos) || !Board.posInsideBoard(toPos) || fromPos == toPos) false
    else fromPos.rowDiff(toPos) == fromPos.colDiff(toPos)
  }

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = {
    (1 to 7).flatMap(ofs =>
      Seq(fromPos.move(ofs, ofs), fromPos.move(-ofs, ofs), fromPos.move(ofs, -ofs), fromPos.move(-ofs, -ofs)))
            .filter(Board.posInsideBoard)
            .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
            .toSet
  }
}

object Rook extends Piece { // tower
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

case object Queen extends Piece {
  val symbol = "Q"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

case object King extends Piece {
  val symbol = "K"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean =
    Board.posInsideBoard(fromPos)
        && Board.posInsideBoard(toPos)
        && fromPos != toPos
        && (fromPos.colDiff(toPos).abs <= 1 && fromPos.rowDiff(toPos).abs <= 1)

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] =
    Set(fromPos.move(-1, 1), fromPos.move(0, 1), fromPos.move(1, 1),
        fromPos.move(-1, 0), fromPos.move(1, 0),
        fromPos.move(-1, -1), fromPos.move(0, -1), fromPos.move(1, -1))
        .filter(Board.posInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
}
