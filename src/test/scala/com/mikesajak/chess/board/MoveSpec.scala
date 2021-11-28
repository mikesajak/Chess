package com.mikesajak.chess.board

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.piece.*

class MoveSpec extends UnitTestSpec {
  "Move colDiff" should "be column difference between toPos and fromPos" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         piece <- List(Pawn, Rook, Knight, Bishop, Queen, King);
         captureAllowed <- List(true, false);
         onlyCapture <- List(true, false) if captureAllowed;
         move = Move(piece, fromPos, toPos, captureAllowed, onlyCapture)) {
      withClue(s"Checking move: $move") {
        move.colDiff should be(toPos.col - fromPos.col)
      }
    }
  }

  "Move rowDiff" should "be row difference between toPos and fromPos" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         piece <- List(Pawn, Rook, Knight, Bishop, Queen, King);
         captureAllowed <- List(true, false);
         onlyCapture <- List(true, false) if captureAllowed;
         move = Move(piece, fromPos, toPos, captureAllowed, onlyCapture)) {
      withClue(s"Checking move: $move") {
        move.rowDiff should be(toPos.row - fromPos.row)
      }
    }
  }
}
