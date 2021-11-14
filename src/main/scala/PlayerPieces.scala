package com.mikesajak.chess

case class PlayerPieces(alivePieces: Set[PiecePosition]) {
  private val rowsMap =
    alivePieces.map(p => (p.position.row, p))
               .groupBy(rowToPiecePos => rowToPiecePos._1)
               .map(rowToAllRowPieces => rowToAllRowPieces._1 -> rowToAllRowPieces._2.toSeq.sortBy(p => p._2.position.col)
                                                                                  .map(colToPiecePosition => colToPiecePosition._2)
                                                                                  .map(piecePosition => piecePosition.position.col -> piecePosition)
                                                                                  .toMap)

  def pieceOnPos(pos: Position): Option[Piece] = pieceOnPos(pos.row, pos.col)

  def pieceOnPos(row: Int, col: Int): Option[Piece] = {
    rowsMap.get(row)
           .flatMap(r => r.get(col))
           .map(_.piece)
  }

  def getPieces(piece: Piece): Set[PiecePosition] =
    alivePieces.filter(piecePos => piecePos.piece == piece)
}

case class PiecePosition(piece: Piece, position: Position)
