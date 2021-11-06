package com.mikesajak.chess

case class Position(col: Int, row: Int) {
  def move(horizontal: Int, vertical: Int): Position = Position(col + horizontal, row + vertical)

  def colDiff(otherPos: Position): Int = otherPos.col - col

  def rowDiff(otherPos: Position): Int = otherPos.row - row

  override def toString: String = s"${mapToLetter(col)}$row"

  private def mapToLetter(idx: Int) = ('a' + idx).toChar
}
