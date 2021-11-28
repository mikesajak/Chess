package com.mikesajak.chess

case class Move(piece: Piece, fromPos: Position, toPos: Position, captureAllowed: Boolean, onlyCapture: Boolean = false) {
  assert(!onlyCapture || captureAllowed)

  def colDiff: Int = fromPos.colDiff(toPos)
  def rowDiff: Int = fromPos.rowDiff(toPos)
}
