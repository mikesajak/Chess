package com.mikesajak.chess.board

case class Position(col: Int, row: Int) {
  def move(horizontal: Int, vertical: Int): Position = Position(col + horizontal, row + vertical)

  def colDiff(otherPos: Position): Int = otherPos.col - col

  def rowDiff(otherPos: Position): Int = otherPos.row - row

  override def toString: String =
    if (isInsideBoard) s"${mapToLetter(col)}$row"
    else s"($col, $row)"

  private def mapToLetter(idx: Int) = ('a' + idx).toChar

  def isInsideBoard: Boolean = col >= 0 && col <= 7 && row >= 0 && row <= 7

  def swapSide: Position = Position(7 - col, 7 - row)
}

object Position {
  def swapSide(col: Int, row: Int): Position = Position(7 - col, 7 - row)
}
