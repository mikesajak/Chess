package com.mikesajak.chess

class PawnSpec extends UnitTestSpec {
  "A Pawn move validation" should "accept regular 1-field forward opening move" in {
    val pawn = new Pawn
    for (col <- 0 to 7;
         fromPos = Position(col, 1);
         toPos = Position(col, 2)) {
      assertValidMove(pawn, fromPos, toPos, firstMove = true)
    }
  }

  it should "accept regular 2-field forward opening move" in {
    val pawn = new Pawn
    for (col <- 0 to 7;
         fromPos = Position(col, 1);
         toPos = Position(col, 3)) {
      assertValidMove(pawn, fromPos, toPos, firstMove = true)
    }
  }

  it should "not accept any opening move from row other than 1" in {
    val pawn = new Pawn

    for (fromPos <- allPositionsOnBoard if fromPos.row != 1) {
      for (toPos <- allPositionsOnBoard) {
        assertInvalidMove(pawn, fromPos, toPos, firstMove = true)
      }
    }
  }

  it should "not accept any move to outside board" in {
    val pawn = new Pawn
    for (fromPos <- allPositionsOnBoard) {
      for (toCol <- 0 to 7) {
        assertInvalidMove(pawn, fromPos, Position(toCol, -1), firstMove = false)
        assertInvalidMove(pawn, fromPos, Position(toCol, -8), firstMove = false)
      }

      for (toRow <- 0 to 7) {
        assertInvalidMove(pawn, fromPos, Position(-1, toRow), firstMove = false)
        assertInvalidMove(pawn, fromPos, Position(8, toRow), firstMove = false)
      }
    }
  }

  ignore should "not accept any move from outside board" in {
    val pawn = new Pawn
    for (toPos <- allPositionsOnBoard) {
      for (fromCol <- 0 to 7) {
        assertInvalidMove(pawn, Position(fromCol, -1), toPos, firstMove = false)
        assertInvalidMove(pawn, Position(fromCol, 8),toPos,  firstMove = false)
      }

      for (toRow <- 0 to 7) {
        assertInvalidMove(pawn, Position(-1, toRow), toPos, firstMove = false)
        assertInvalidMove(pawn, Position(8, toRow), toPos, firstMove = false)
      }
    }
  }

  it should "accept all 1-field forward regular moves" in {
    val pawn = new Pawn
    for (fromPos <- allPositionsOnBoard if fromPos.row < 7;
         toPos = fromPos.move(0, 1)) {
      assertValidMove(pawn, fromPos, toPos, firstMove = false)
    }
  }

  it should "accept all 1-field capture (diagonal) moves" in {
    val pawn = new Pawn
    for (fromCol <- 0 to 7;
         fromRow <- 0 to 6;
         fromPos = Position(fromCol, fromRow)) {
      if (fromCol > 0) {
        val toPos = Position(fromCol - 1, fromRow + 1)
        assertValidMove(pawn, fromPos, toPos, firstMove = false)
      }

      if (fromCol < 7) {
        val toPos = Position(fromCol + 1, fromRow + 1)
        assertValidMove(pawn, fromPos, toPos, firstMove = false)
      }
    }
  }


  private def assertValidMove(pawn: Pawn, fromPos: Position, toPos: Position, firstMove: Boolean): Unit =
    assertValidMove(pawn, fromPos, toPos, firstMove, validState = true)

  private def assertInvalidMove(pawn: Pawn, fromPos: Position, toPos: Position, firstMove: Boolean): Unit  =
    assertValidMove(pawn, fromPos, toPos, firstMove, validState = false)

  private def assertValidMove(pawn: Pawn, fromPos: Position, toPos: Position, firstMove: Boolean, validState: Boolean): Unit =
    withClue(s"fromPos: $fromPos, toPos: $toPos") {
      pawn.isValidMove(fromPos, toPos, firstMove) should be(validState)
    }

  private def allPositionsOnBoard: Seq[Position] =
    for (col <- 0 to 7;
         row <- 0 to 7) yield Position(col, row)

}
