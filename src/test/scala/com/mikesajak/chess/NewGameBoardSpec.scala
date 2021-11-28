package com.mikesajak.chess

import com.mikesajak.chess.board.*
import com.mikesajak.chess.piece.*

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

  private def checkPawns(pieces: PlayerState): Unit = {
    val pawns = pieces.getPieces(Pawn)
    pawns.size should be (8)

    pawns.foreach(posPiece => posPiece._1.row == 1)
    pawns.map(posPiece => posPiece._1.col).toSeq.sorted should be (Seq(0, 1, 2, 3, 4, 5, 6, 7))
  }

  private def checkRooks(pieces: PlayerState, color: PlayerSide): Unit = {
    val rooks = pieces.getPieces(Rook)
    rooks.size should be (2)

    val (rook1Pos, rook2Pos) = getPiecePair(rooks)

    rook1Pos should be (Position(0, 0))
    rook2Pos should be (Position(7, 0))
  }

  private def checkKnights(pieces: PlayerState, color: PlayerSide): Unit = {
    val knights = pieces.getPieces(Knight)
    knights.size should be (2)

    val (knight1Pos, knight2Pos) = getPiecePair(knights)

    knight1Pos should be (Position(1, 0))
    knight2Pos should be (Position(6, 0))
  }

  private def checkBishops(pieces: PlayerState, color: PlayerSide): Unit = {
    val bishops = pieces.getPieces(Bishop)
    bishops.size should be (2)

    val (bishop1Pos, bishop2Pos) = getPiecePair(bishops)

    bishop1Pos should be (Position(2, 0))
    bishop2Pos should be (Position(5, 0))
  }

  private def checkQueen(pieces: PlayerState, color: PlayerSide): Unit = {
    val queenPositions = pieces.getPieces(Queen).keys
    queenPositions.size should be (1)

    val queen = queenPositions.head
    queen should be (Position(3, 0))
  }

  private def checkKing(playerPieces: PlayerState, color: PlayerSide): Unit = {
    val kingPositions = playerPieces.getPieces(King).keys
    kingPositions.size should be (1)
    val king = kingPositions.head

    king should be (Position(4, 0))
  }

  private def getPiecePair(posPieceMap: Map[Position, Piece]): (Position, Position) = {
    val sortedPieces = posPieceMap.toSeq.sortBy(posPiece => posPiece._1.col)
    (sortedPieces.head._1, sortedPieces(1)._1)
  }
}
