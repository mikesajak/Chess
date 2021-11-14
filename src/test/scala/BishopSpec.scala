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

  "Valid moves" should "contain only forward diagonal moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos=$fromPos: ") {
        val expectedMoves = (1 to 7).flatMap(ofs => Set(fromPos.move(ofs, ofs)) ++ Set(fromPos.move(-ofs, ofs)) ++
                                                    Set(fromPos.move(ofs, -ofs)) ++ Set(fromPos.move(-ofs, -ofs)))
                                    .filter(Board.posInsideBoard)
                                    .map(toPos => Move(Bishop, fromPos, toPos, captureAllowed = true))
                                    .toSet
        Bishop.validMoves(fromPos, firstMove) should be (expectedMoves)
      }
    }
  }

}