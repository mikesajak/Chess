package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.{Board, Move, Position}

class PawnSpec extends UnitTestSpec {
  "A Pawn move validation" should "accept regular 1-field forward opening move" in {
    for (col <- 0 to 7;
         fromPos = Position(col, 1);
         toPos = Position(col, 2)) {
      assertValidMove(fromPos, toPos, firstMove = true)
    }
  }

  it should "accept regular 2-field forward opening move" in {
    for (col <- 0 to 7;
         fromPos = Position(col, 1);
         toPos = Position(col, 3)) {
      assertValidMove(fromPos, toPos, firstMove = true)
    }
  }

  it should "not accept any opening move from row other than 1" in {
    for (fromPos <- allPositionsOnBoard if fromPos.row != 1) {
      for (toPos <- allPositionsOnBoard) {
        assertInvalidMove(fromPos, toPos, firstMove = true)
      }
    }
  }

  it should "not accept any move to outside board" in {
    for (fromPos <- allPositionsOnBoard) {
      for (toCol <- 0 to 7;
           toRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000)) {
        assertInvalidMove(fromPos, Position(toCol, toRow), firstMove = false)
      }

      for (toRow <- 0 to 7;
           toCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000)) {
        assertInvalidMove(fromPos, Position(toCol, toRow), firstMove = false)
      }
    }
  }

  it should "not accept any move from outside board" in {
    for (toPos <- allPositionsOnBoard) {
      for (fromCol <- 0 to 7;
           fromRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000)) {
        assertInvalidMove(Position(fromCol, fromRow), toPos, firstMove = false)
      }

      for (fromRow <- 0 to 7;
           fromCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000)) {
        assertInvalidMove(Position(fromCol, fromRow), toPos, firstMove = false)
      }
    }
  }

  it should "accept all 1-field forward regular moves" in {
    for (fromPos <- allPositionsOnBoard if fromPos.row < 7;
         toPos = fromPos.move(0, 1)) {
      assertValidMove(fromPos, toPos, firstMove = false)
    }
  }

  it should "accept all 1-field capture (diagonal) moves" in {
    for (fromCol <- 0 to 7;
         fromRow <- 0 to 6;
         fromPos = Position(fromCol, fromRow)) {
      if (fromCol > 0) {
        val toPos = Position(fromCol - 1, fromRow + 1)
        assertValidMove(fromPos, toPos, firstMove = false)
      }

      if (fromCol < 7) {
        val toPos = Position(fromCol + 1, fromRow + 1)
        assertValidMove(fromPos, toPos, firstMove = false)
      }
    }
  }

  "Valid moves" should "contain 1 and 2 field moves forward and 1-field diagonal capture moves for first move" in {
    for (fromPos <- Board.allPositionsForRow(1)) {
      withClue(s"Checking fromPos: $fromPos") {

        val regularMoves = Set(Move(Pawn, fromPos, fromPos.move(0, 1), captureAllowed = false),
                               Move(Pawn, fromPos, fromPos.move(0, 2), captureAllowed = false))
        val leftCaptureMove = if (fromPos.col > 0) Set(Move(Pawn, fromPos, fromPos.move(-1, 1), captureAllowed = true, onlyCapture = true))
                              else Set()
        val rightCaptureMove = if (fromPos.col < 7) Set(Move(Pawn, fromPos, fromPos.move(1, 1), captureAllowed = true, onlyCapture = true))
                               else Set()

        val captureMoves = leftCaptureMove ++ rightCaptureMove

        Pawn.validMoves(fromPos, firstMove = true) should be (regularMoves ++ captureMoves)
      }
    }
  }

  it should "contain only 1 field move forward and 1-field diagonal capture moves for non-first move" in {
    for (fromPos <- Board.allPositionsForRow(1)) {
      withClue(s"Checking fromPos: $fromPos") {
        val regularMoves = Set(Move(Pawn, fromPos, fromPos.move(0, 1), captureAllowed = false))
        val leftCaptureMove = if (fromPos.col > 0) Set(Move(Pawn, fromPos, fromPos.move(-1, 1), captureAllowed = true, onlyCapture = true))
                              else Set()
        val rightCaptureMove = if (fromPos.col < 7) Set(Move(Pawn, fromPos, fromPos.move(1, 1), captureAllowed = true, onlyCapture = true))
                               else Set()

        val captureMoves = leftCaptureMove ++ rightCaptureMove

        Pawn.validMoves(fromPos, firstMove = false) should be (regularMoves ++ captureMoves)
      }
    }
  }

  private def assertValidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Unit =
    assertValidMove(fromPos, toPos, firstMove, validState = true)

  private def assertInvalidMove(fromPos: Position, toPos: Position, firstMove: Boolean): Unit  =
    assertValidMove(fromPos, toPos, firstMove, validState = false)

  private def assertValidMove(fromPos: Position, toPos: Position, firstMove: Boolean, validState: Boolean): Unit =
    withClue(s"fromPos: $fromPos, toPos: $toPos") {
      Pawn.isValidMove(fromPos, toPos, firstMove) should be(validState)
    }

  private def allPositionsOnBoard: Seq[Position] =
    for (col <- 0 to 7;
         row <- 0 to 7) yield Position(col, row)

}
