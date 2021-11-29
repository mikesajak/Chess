package com.mikesajak.chess.piece

import com.mikesajak.chess.UnitTestSpec
import com.mikesajak.chess.board.{Board, Position}

class QueenSpec extends UnitTestSpec {
  "A Queen move validation" should "accept diagonal moves in all dirs" in {
    for (fromPos <- Board.allPositions;
         toOfs <- -7 to 7;
         toPos = fromPos.move(toOfs, toOfs)
         if fromPos != toPos
         if Board.posInsideBoard(toPos);
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be (true)
      }
    }
  }

  it should "accept any horizotal moves" in {
    for (fromPos <- Board.allPositions;
         toOfs <- -7 to 7;
         toPos = fromPos.move(toOfs, 0)
         if fromPos != toPos
         if Board.posInsideBoard(toPos);
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be (true)
      }
    }
  }

  it should "accept any vertical moves" in {
    for (fromPos <- Board.allPositions;
         toOfs <- -7 to 7;
         toPos = fromPos.move(0, toOfs)
         if fromPos != toPos
         if Board.posInsideBoard(toPos);
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be (true)
      }
    }
  }

//  x  O  x  O  x  O  O  O
//  O  x  x  x  O  O  O  O
//  x  x  X  x  x  x  x  x
//  O  x  x  x  O  O  O  O
//  x  O  x  O  x  O  O  O
//  O  O  x  O  O  x  O  O
//  O  O  x  O  O  O  x  O
//  O  O  x  O  O  O  O  x

  it should "not accept non perpendicular or diagonal moves" in {
    for (fromPos <- Board.allPositions;
         toPos <- Board.allPositions
         if fromPos.col != toPos.col // not horizontal
         if fromPos.row != toPos.row // not horizontal
         if fromPos.colDiff(toPos).abs != fromPos.rowDiff(toPos).abs; // not diagonal
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be(false)
      }
    }
  }

  it should "not accept any move outside board" in {
    for (fromPos <- Board.allPositions;
         toRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         toCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         toPos = Position(toCol, toRow);
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be(false)
      }
    }
  }

  it should "not accept any move from outside board" in {
    for (toPos <- Board.allPositions;
         fromRow <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         fromCol <- List(-1000, -100, -10, -2, -1, 8, 9, 20, 100, 1000);
         fromPos = Position(fromCol, fromRow);
         firstMove <- List(true, false)) {
      withClue(s"Checking $fromPos -> $toPos: ") {
        Queen.isValidMove(fromPos, toPos, firstMove) should be (false)
      }
    }
  }

  "Queen valid moves" should "not be empty for any valid position" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         queenMoves = Queen.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $fromPos/first($firstMove): ") {
        queenMoves should not be empty
      }
    }
  }

  it should "contain only horizontal or vertical moves" in {
    for (fromPos <- Board.allPositions;
         firstMove <- List(true, false);
         move <- Queen.validMoves(fromPos, firstMove)) {
      withClue(s"Checking $move: ") {
        move.piece should be (Rook)
        move.fromPos should be (fromPos)

        if (move.rowDiff == 0) move.colDiff should not be 0
        else move.colDiff should be (0)

        move.captureAllowed should be (true)
        move.onlyCapture should be (false)
      }
    }
  }
}
