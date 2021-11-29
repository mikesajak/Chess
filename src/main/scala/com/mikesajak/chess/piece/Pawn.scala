package com.mikesajak.chess.piece

import com.mikesajak.chess.board.{Board, Move, Position}

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
                                                                      .map(toPos => Move(this,
                                                                                         fromPos,
                                                                                         toPos,
                                                                                         captureAllowed = false))

    val captureMoves = captureTargetPositions.view
                                             .filter(Board.posInsideBoard)
                                             .map(toPos => Move(this,
                                                                fromPos,
                                                                toPos,
                                                                captureAllowed = true,
                                                                onlyCapture = true))

    (regularMoves ++ captureMoves).toSet
  }

}
