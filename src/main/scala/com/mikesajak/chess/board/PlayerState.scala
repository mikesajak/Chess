package com.mikesajak.chess.board

import com.mikesajak.chess.board.Position
import com.mikesajak.chess.piece.Piece

object PlayerState {
  def apply(positionsToPieces: (Position, Piece)*) = new PlayerState(Map(positionsToPieces:_*))
}

case class PlayerState(alivePieces: Map[Position, Piece]) {
  def pieceOnPos(pos: Position): Option[Piece] =
    alivePieces.get(pos)

  def pieceOnPos(col: Int, row: Int): Option[Piece] =
    alivePieces.get(Position(col, row))

  def getPieces(piece: Piece): Map[Position, Piece] =
    alivePieces.filter(posPiece => posPiece._2 == piece)
}
