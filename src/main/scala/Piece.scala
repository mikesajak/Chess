package com.mikesajak.chess

enum PieceType(val symbol: String):
  case Pawn   extends PieceType("P")
  case Knight extends PieceType("H")
  case Bishop extends PieceType("B")
  case Rook   extends PieceType("R")
  case Queen  extends PieceType("Q")
  case King   extends PieceType("K")

object Piece {
  def isMoveInsideBoard(toPos: Position): Boolean =
    toPos.row >=0 && toPos.row < 8 && toPos.col >= 0 && toPos.col < 8
}

trait Piece {
  def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean
  def validMoves(fromPos: Position, firstMove: Boolean): Set[Move]
  def pieceType: PieceType

  override def toString: String = pieceType.symbol

  override def equals(obj: Any): Boolean = obj match {
    case piece: Piece => piece.pieceType == pieceType
    case _ => false
  }

  override def hashCode(): Int = pieceType.hashCode
}

case class Move(piece: Piece, fromPos: Position, toPos: Position, captureAllowed: Boolean)

class Pawn extends Piece {
  import Piece.*

  val pieceType: PieceType = PieceType.Pawn

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (firstMove && fromPos.row != 1) false
    else {
      isMoveInsideBoard(toPos)
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
    val captureTargetPositions = Set(fromPos.move(-1, 0),
                                     fromPos.move(1, 0))

    val regularMoves = (regularTargetPos ++ regularFirstMoveTargetPos)
        .filter(Piece.isMoveInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = false))

    val captureMoves = captureTargetPositions
        .filter(Piece.isMoveInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))

    regularMoves ++ captureMoves
  }

}

class Knight extends Piece { // horse
  val pieceType: PieceType = PieceType.Knight

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

class Bishop extends Piece { // Laufer
  val pieceType: PieceType = PieceType.Bishop

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

class Rook extends Piece { // tower
  val pieceType: PieceType = PieceType.Rook

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

class Queen extends Piece {
  val pieceType: PieceType = PieceType.Queen

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

class King extends Piece {
  val pieceType: PieceType = PieceType.King

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean =
    Piece.isMoveInsideBoard(toPos)
        && fromPos != toPos
        && (math.abs(fromPos.colDiff(toPos)) <= 1 || math.abs(fromPos.rowDiff(toPos)) <= 1)

  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] =
    Set(fromPos.move(-1, 1), fromPos.move(0, 1), fromPos.move(1, 1),
        fromPos.move(-1, 0), fromPos.move(1, 0),
        fromPos.move(-1, -1), fromPos.move(0, -1), fromPos.move(1, -1))
        .filter(Piece.isMoveInsideBoard)
        .map(toPos => Move(this, fromPos, toPos, captureAllowed = true))
}

