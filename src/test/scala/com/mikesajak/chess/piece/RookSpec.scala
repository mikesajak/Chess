package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.{Board, Position}

class RookSpec extends UnitTestSpec {
  "A Rook move validation" should "accept only horizontal and vertical moves" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        val isHorizontalMove = toPos.colDiff(fromPos) != 0 && toPos.rowDiff(fromPos) == 0
        val isVerticalMove = toPos.rowDiff(fromPos) != 0 && toPos.colDiff(fromPos) == 0
        Rook.isValidMove(fromPos, toPos, firstMove) should be(fromPos != toPos && (isHorizontalMove || isVerticalMove))
      }
    }
  }

  it should "not accept any move outside board" in {
    for (fromPos <- Board.allPositions;
         toRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         toCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         toPos = Position(toCol, toRow);
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        Rook.isValidMove(fromPos, toPos, firstMove) should be(false)
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
        Rook.isValidMove(fromPos, toPos, firstMove) should be (false)
      }
    }
  }

  "Rook valid moves" should "contain only horizontal or vertical moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- Rook.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move:") {
        move.piece should be (Rook)
        move.fromPos should be (fromPos)

        if (move.rowDiff == 0) move.colDiff should not be 0
        else move.colDiff should be (0)

        move.captureAllowed should be (true)
        move.onlyCapture should be (false)
      }
    }
  }

  it should "contain not contain moves outside board" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- Rook.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move: ") {
        move.piece should be (Rook)
        move.fromPos should be (fromPos)
        move.toPos should not be fromPos

        val isInsideBoard = move.toPos.col >= 0 && move.toPos.col <= 7
            && move.toPos.row >= 0 && move.toPos.row <= 7

        isInsideBoard should be (true)
      }
    }
  }

}
