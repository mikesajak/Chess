package com.mikesajak.chess

class BishopSpec extends UnitTestSpec {
  "A Bishop move validation" should "accept only diagonal moves" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        val isDiagonalMove = toPos.colDiff(fromPos) == toPos.rowDiff(fromPos)
        Bishop.isValidMove(fromPos, toPos, firstMove) should be(fromPos != toPos && isDiagonalMove)
      }
    }
  }

  it should "not accept any move outside board" in {
    for (fromPos <- Board.allPositions;
         outside <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         inside <- 0 to 7;
         firstMove <- List(true, false)) {
      val toPos = Position(outside, inside)
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        Bishop.isValidMove(fromPos, toPos, firstMove) should be(false)
      }

      val toPos2 = Position(inside, outside)
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos2") {
        Bishop.isValidMove(fromPos, toPos2, firstMove) should be(false)
      }
    }
  }

  it should "not accept any move from outside board" in {
    for (toPos <- Board.allPositions;
         outside <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         inside <- 0 to 7;
         firstMove <- List(true, false)) {
      val fromPos = Position(outside, inside)
      withClue(s"Checking fromPos: $fromPos, toPos: $toPos") {
        Bishop.isValidMove(fromPos, toPos, firstMove) should be (false)
      }

      val fromPos2 = Position(inside, outside)
      withClue(s"Checking fromPos: $fromPos2, toPos: $toPos") {
        Bishop.isValidMove(fromPos2, toPos, firstMove) should be (false)
      }
    }
  }

  "Valid moves" should "contain only diagonal moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- Bishop.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move: ") {
        move.piece should be(Bishop)
        move.fromPos should not be move.toPos

        move.colDiff.abs should be (move.rowDiff.abs)

        move.captureAllowed should be (true)
        move.onlyCapture should be (false)
      }
    }
  }

}