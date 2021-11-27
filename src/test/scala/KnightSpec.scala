package com.mikesajak.chess

class KnightSpec extends UnitTestSpec {
  "A Knight move validation" should "accept valid knight jump moves" in {
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
    for (fromPos <- Board.allPositionsForRow(0) ++ Board.allPositionsForRow(1) ++
        Board.allPositionsForRow(6) ++ Board.allPositionsForRow(7) ++
        Board.allPositionsForCol(0) ++ Board.allPositionsForCol(1) ++
        Board.allPositionsForCol(6) ++ Board.allPositionsForCol(7);
         toPos <- knightMovesFrom(fromPos)
         if !Board.posInsideBoard(toPos);
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

  "Knight valid moves" should "contain only horizontal or vertical moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false)) {
      withClue(s"Checking fromPos=$fromPos: ") {
        val expectedMoves = knightMovesFrom(fromPos)
            .filter(Board.posInsideBoard)
            .map(toPos => Move(Knight, fromPos, toPos, captureAllowed = true))

        Knight.validMoves(fromPos, firstMove) should be (expectedMoves)
      }
    }
  }

  private def knightMovesFrom(fromPos: Position) =
    Set(fromPos.move(-1, -2),
        fromPos.move(-1, 2),
        fromPos.move(1, -2),
        fromPos.move(1, 2),
        fromPos.move(-2, -1),
        fromPos.move(-2, 1),
        fromPos.move(2, -1),
        fromPos.move(2, 1))

}
