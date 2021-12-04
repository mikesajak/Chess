package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.{Board, Position}

class KingSpec extends UnitTestSpec {
  "King move validation" should "accept only 1-field moves" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        val is1FieldMove = toPos.colDiff(fromPos).abs <= 1 && toPos.rowDiff(fromPos).abs <= 1
        King.isValidMove(fromPos, toPos, firstMove) should be(fromPos != toPos && is1FieldMove)
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
        King.isValidMove(fromPos, toPos, firstMove) should be(false)
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
        King.isValidMove(fromPos, toPos, firstMove) should be (false)
      }
    }
  }

  "King valid moves" should "contain only 1-square moves in all directions" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- King.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move: ") {
        move.piece should be (King)
        move.toPos should not be move.fromPos
        move.toPos.rowDiff(move.fromPos).abs should be <= 1
        move.toPos.colDiff(move.fromPos).abs should be <= 1
        move.captureAllowed should be (true)
        move.onlyCapture should be (false)
      }
    }
  }

}
