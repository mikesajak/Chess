package com.mikesajak.chess

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

  "Valid moves" should "contain only horizontal or vertical moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos=$fromPos: ") {
        val expectedMoves =
          ((1 to fromPos.row).map(row => fromPos.move(0, -row)) ++
              (1 to 7-fromPos.row).map(row => fromPos.move(0, row)) ++
              (1 to fromPos.col).map(col => fromPos.move(-col, 0)) ++
              (1 to 7-fromPos.col).map(col => fromPos.move(col, 0)))
        .map(toPos => Move(Rook, fromPos, toPos, captureAllowed = true))
        .toSet

        Rook.validMoves(fromPos, firstMove) should be (expectedMoves)
      }
    }
  }

}
