package com.mikesajak.chess

class NewGameBoardSpec extends UnitTestSpec {
  "New board factory method" should "create board for new game" in {
    val board = Board.newGame()

    board.moves.isEmpty should be (true)

    List((PlayerSide.White, board.whitePieces), (PlayerSide.Black, board.blackPieces))
        .foreach { case (color, pieces) =>
          withClue(s"Checking $color pawns:") {
            checkPawns(pieces)
          }

          withClue(s"Checking $color rooks:") {
            checkRooks(pieces, color)
          }

          withClue(s"Checking $color knights:") {
            checkKnights(pieces, color)
          }

          withClue(s"Checking $color bishops:") {
            checkBishops(pieces, color)
          }

          withClue(s"Checking $color queen:") {
            checkQueen(pieces, color)
          }

          withClue(s"Checking $color king:") {
            checkKing(pieces, color)
          }
        }
  }

  private def checkPawns(pieces: PlayerPieces): Unit = {
    val pawns = pieces.getPieces(Pawn)
    pawns.size should be (8)

    pawns.foreach(piecePos => piecePos.position.row == 1)
    pawns.map(piecePos => piecePos.position.col).toSeq.sorted should be (Seq(0, 1,2,3,4,5,6,7))
  }

  private def checkRooks(pieces: PlayerPieces, color: PlayerSide): Unit = {
    val rooks = pieces.getPieces(Rook)
    rooks.size should be (2)

    val (rook1, rook2) = getPiecePair(rooks)

    rook1.position should be (Position(0, 0))
    rook2.position should be (Position(7, 0))
  }

  private def checkKnights(pieces: PlayerPieces, color: PlayerSide): Unit = {
    val knights = pieces.getPieces(Knight)
    knights.size should be (2)

    val (knight1, knight2) = getPiecePair(knights)

    knight1.position should be (Position(1, 0))
    knight2.position should be (Position(6, 0))
  }

  private def checkBishops(pieces: PlayerPieces, color: PlayerSide): Unit = {
    val bishops = pieces.getPieces(Bishop)
    bishops.size should be (2)

    val (bishop1, bishop2) = getPiecePair(bishops)

    bishop1.position should be (Position(2, 0))
    bishop2.position should be (Position(5, 0))
  }

  private def checkQueen(pieces: PlayerPieces, color: PlayerSide): Unit = {
    val queens = pieces.getPieces(Queen)
    queens.size should be (1)

    val queen = queens.head
    queen.position should be (Position(3, 0))
  }

  private def checkKing(playerPieces: PlayerPieces, color: PlayerSide): Unit = {
    val kings = playerPieces.getPieces(King)
    kings.size should be (1)
    val king = kings.head

    king.position should be (Position(4, 0))
  }

  private def getPiecePair(pieces: Set[PiecePosition]): (PiecePosition, PiecePosition) = {
    val sortedPieces = pieces.toSeq.sortBy(_.position.col)
    (sortedPieces.head, sortedPieces(1))
  }
}
