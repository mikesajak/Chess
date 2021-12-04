package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.{Board, Position}

class KnightSpec extends UnitTestSpec {
  "A Knight move validation" should "accept L-jump moves" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        val isHorizontalJump = fromPos.rowDiff(toPos).abs == 1 && fromPos.colDiff(toPos).abs == 2
        val isVerticalJump = fromPos.colDiff(toPos).abs == 1 && fromPos.rowDiff(toPos).abs == 2
        Knight.isValidMove(fromPos, toPos, firstMove) should be(fromPos != toPos && (isHorizontalJump || isVerticalJump))
      }
    }
  }

  it should "not accept any move outside board" in {
    for (fromPos <- Board.allPositionsForRows(0, 1, 6, 7) ++ Board.allPositionsForCols(0, 1, 6, 7);
         toPos <- Board.allPositionsForRows(-1000, -100, -10, -1, -2, 8, 9, 20, 100, 1000)
             ++ Board.allPositionsForCols(-1000, -100, -10, -1, -2, 8, 9, 20, 100, 1000);
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        Knight.isValidMove(fromPos, toPos, firstMove) should be(false)
      }
    }
  }

  it should "not accept any move from outside board" in {
    for (toPos <- Board.allPositions;
         fromRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         fromCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         fromPos = Position(fromCol, fromRow);
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        Knight.isValidMove(fromPos, toPos, firstMove) should be (false)
      }
    }
  }

  "Knight valid moves" should "contain only L-shape capture moves" in {
     for (fromPos <- Board.allPositions;
          firstMove <- List(true, false);
          move <- Knight.validMoves(fromPos, firstMove)) {
       withClue(s"Checking $move:") {
         move.piece should be (Knight)
         move.fromPos should not be move.toPos

         val colDiff = move.colDiff.abs
         val rowDiff = move.rowDiff.abs

         colDiff should be <= 2
         rowDiff should be <= 2

         if (colDiff == 2) rowDiff should be (1)
         else rowDiff should be (2)

         move.captureAllowed should be (true)
         move.onlyCapture should be (false)
       }
     }
  }
  
}
