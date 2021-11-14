package com.mikesajak.chess

object Piece {
  def isMoveInsideBoard(toPos: Position): Boolean =
    toPos.row >=0 && toPos.row < 8 && toPos.col >= 0 && toPos.col < 8

  def allPieceTypes: Seq[Piece] = Seq(Pawn, Rook, Knight, Bishop, Queen, Knight)
}

trait Piece {
  def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean
  def validMoves(fromPos: Position, firstMove: Boolean): Set[Move]
  def symbol: String

  override def toString: String = symbol
}

case class Move(piece: Piece, fromPos: Position, toPos: Position, captureAllowed: Boolean)

case object Pawn extends Piece {
  import Piece.*

  val symbol = "P"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = {
    if (fromPos.col < 0 || fromPos.col > 7
        || fromPos.row < 0 || fromPos.row > 7) false
    else if (firstMove && fromPos.row != 1) false
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

case object Knight extends Piece { // horse
  val symbol = "H"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

case object Bishop extends Piece { // Laufer
  val symbol = "B"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

object Rook extends Piece { // tower
  val symbol = "R"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

case object Queen extends Piece {
  val symbol = "Q"

  override def isValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Boolean = false
  override def validMoves(fromPos: Position, firstMove: Boolean): Set[Move] = Set()
}

case object King extends Piece {
  val symbol = "K"

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
