package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.Board

class PieceSpec extends UnitTestSpec{
  "Piece valid moves" should "contain not contain moves outside board" in {
    for (piece <- List(Pawn, Rook, Knight, Bishop, Queen, Knight);
         fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- piece.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move: ") {
        move.piece should be (piece)
        move.fromPos should be (fromPos)
        move.toPos should not be fromPos

        val isInsideBoard = move.toPos.col >= 0 && move.toPos.col <= 7
            && move.toPos.row >= 0 && move.toPos.row <= 7

        isInsideBoard should be (true)
      }
    }
  }
}
