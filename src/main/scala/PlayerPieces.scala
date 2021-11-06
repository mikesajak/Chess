package com.mikesajak.chess

class PlayerPieces(val alivePieces: Set[PiecePosition]) {
  private val rowsMap = alivePieces.map(p => (p.position.row, p))
                           .groupBy(_._1)
                           .map(t => t._2.toSeq.sortBy(p => p._2.position.col)
                                      .map(_._2)
                                      .zipWithIndex
                                      .map(elem => (elem._2, elem._1))
                                      .toMap)
                           .zipWithIndex
                           .map(elem => (elem._2, elem._1))
                           .toMap

  def pieceOnPos(pos: Position): Option[Piece] = pieceOnPos(pos.row, pos.col)

  def pieceOnPos(row: Int, col: Int): Option[Piece] = {
    rowsMap.get(row)
           .flatMap(r => r.get(col))
           .map(_.piece)
  }

  def getPieces(pieceType: PieceType): Set[PiecePosition] =
    alivePieces.filter(piecePos => piecePos.piece.pieceType == pieceType)
}


case class PiecePosition(piece: Piece, position: Position)

/*
Black
   0  1  2  3  4  5  6  7
7  R  H  B  K  Q  B  H  R
6  P  P  P  P  P  P  P  P
5
4
3
2
1  P  P  P  P  P  P  P  P
0  R  H  B  Q  K  B  H  R
   0  1  2  3  4  5  6  7
White
*/

object PlayerPieces {
  def createPlayerPieces(): PlayerPieces = PlayerPieces(createPiecesForNewGame())

  def createPiecesForNewGame(): Set[PiecePosition] = {
    Set(
      piece("P", 0, 1),
      piece("P", 1, 1),
      piece("P", 2, 1),
      piece("P", 3, 1),
      piece("P", 4, 1),
      piece("P", 5, 1),
      piece("P", 6, 1),
      piece("P", 7, 1),

      piece("R", 0, 0),
      piece("H", 1, 0),
      piece("B", 2, 0),
      piece("Q", 3, 0),
      piece("K", 4, 0),
      piece("B", 5, 0),
      piece("H", 6, 0),
      piece("R", 7, 0),
    )
    
  }

  def piece(symbol: String, col: Int, row: Int): PiecePosition = PiecePosition(piece(symbol), Position(col, row))

  def piece(symbol: String): Piece = symbol match {
    case "P" => new Pawn()
    case "R" => new Rook()
    case "H" => new Knight()
    case "B" => new Bishop()
    case "Q" => new Queen()
    case "K" => new King()
    case _ => throw new IllegalArgumentException(s"Unrecognized piece symbol: '$symbol'")
  }
}
