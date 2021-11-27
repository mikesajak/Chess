package com.mikesajak.chess

import org.scalatest.prop.TableDrivenPropertyChecks.*

class BoardSpec extends UnitTestSpec {

  "allPositionsForRow" should "generate positions for specified row only" in {
    for (row <- 0 to 7) {
      val positions = Board.allPositionsForRow(row)

      withClue(s"Checking row: $row") {
        positions should be ((0 to 7).map(col => Position(col, row)))
      }
    }
  }

  "allPositionsForRows" should "generate positions for specified rows only" in {
    val rowCombinations = List(List(0,1),
                               List(0,2),
                               List(0,3),
                               List(0,4),
                               List(0,5),
                               List(0,6),
                               List(0,7),
                               List(0, 2, 4, 6),
                               List(1, 3, 5, 7),
                               List(0, 1, 2, 3, 4, 5, 6, 7))
    for (rows <- rowCombinations) {
      val positions = Board.allPositionsForRows(rows: _*)

      withClue(s"Checking rows: $rows") {
        val expectedPositions = (0 to 7).flatMap(col => rows.map(row => Position(col, row)))
        positions.size should be (expectedPositions.size)
        expectedPositions.foreach(expectedPos => positions should contain (expectedPos))
      }
    }
  }

  "allPositionsForCol" should "generate positions for specified col only" in {
    for (col <- 0 to 7) {
      val positions = Board.allPositionsForCol(col)

      withClue(s"Checking col: $col") {
        positions should be ((0 to 7).map(row => Position(col, row)))
      }
    }
  }

  "allPositionsForCols" should "generate positions for specified cols only" in {
    val colCombinations = List(List(0,1),
                               List(0,2),
                               List(0,3),
                               List(0,4),
                               List(0,5),
                               List(0,6),
                               List(0,7),
                               List(0, 2, 4, 6),
                               List(1, 3, 5, 7),
                               List(0, 1, 2, 3, 4, 5, 6, 7))
    for (cols <- colCombinations) {
      val positions = Board.allPositionsForCols(cols: _*)

      withClue(s"Checking cols: $cols") {
        val expectedPositions = (0 to 7).flatMap(row => cols.map(col => Position(col, row)))
        positions.size should be (expectedPositions.size)
        expectedPositions.foreach(expectedPos => positions should contain (expectedPos))
      }
    }
  }

  "isValid" should "fail for empty board with 2 opposite pieces on the same position" in {
    val invalidCombinations = Seq((PiecePosition(Pawn, Position(1, 1)),
                                      PiecePosition(Pawn, Board.mapToSide(PlayerSide.Black, Position(1,1)))))

    for ((whitePiecePos, blackPiecePos) <- invalidCombinations) {
      val whitePieces = PlayerPieces(Set(whitePiecePos))
      val blackPieces = PlayerPieces(Set(blackPiecePos))
      val board = new Board(whitePieces, blackPieces, Seq())

      withClue(s"Checking board $board:") {
        board.isValid should be(false)
      }
    }
  }

  it should "fail for empty board with 2 the same pieces on the same position" in {
    val whitePieces = PlayerPieces(Set(PiecePosition(Pawn, Position(1, 1)),
                                       PiecePosition(King, Position(1, 1))))
    val blackPieces = PlayerPieces(Set(PiecePosition(Pawn, Position(1, 1))))
    val board = new Board(whitePieces, blackPieces, Seq())

    board.isValid should be (false)
  }

  it should "fail for single white piece outside board" in {
    val whitePieces = PlayerPieces(Set(PiecePosition(Pawn, Position(-1, 1))))
    val blackPieces = PlayerPieces(Set(PiecePosition(Pawn, Position(1, 1))))
    val board = new Board(whitePieces, blackPieces, Seq())

    board.isValid should be (false)
  }

  it should "succeed for new board with standard setup" in {
    val board = Board.newGame()

    board.isValid should be (true)
  }


  "isOccupied" should "be true only for occupied only starting positions on new board" in {
    val board = Board.newGame()

    val expectedOccupiedPositions =
      (Board.allPositionsForRow(0) ++
        Board.allPositionsForRow(1) ++
        Board.allPositionsForRow(6) ++
        Board.allPositionsForRow(7)).toSet

    for (pos <- Board.allPositions) {
      withClue(s"Checking position: $pos") {
        if (expectedOccupiedPositions contains pos)
          board.isOccupied(pos) should be(true)
        else
          board.isOccupied(pos) should be(false)
      }
    }
  }

  it should "detect single piece on board and not any other field" in {

    for (piece <- Piece.allPieceTypes;
         occupiedPos <- Board.allPositions;
         piecePos = PiecePosition(piece, occupiedPos)) {
      for (row <- 0 to 7; col <- 0 to 7;
           checkPos = Position(col, row)) {
        val board = new Board(PlayerPieces(Set(piecePos)), PlayerPieces(Set()), Seq())
        withClue(s"Occupied white $piece, position $occupiedPos, checking position: $checkPos") {
          board.isOccupied(checkPos) should be(occupiedPos == checkPos)
        }
      }
      for (row <- 0 to 7; col <- 0 to 7;
           checkPos = Board.mapToSide(PlayerSide.Black, Position(col, row))) {
        val occupiedBlackPos = Board.mapToSide(PlayerSide.Black, occupiedPos)
        val board = new Board(PlayerPieces(Set()), PlayerPieces(Set(piecePos)), Seq())
        withClue(s"Occupied black $piece, position $occupiedBlackPos, checking position: $checkPos") {
          board.isOccupied(checkPos) should be (occupiedBlackPos == checkPos)
        }
      }
    }
  }

  it should "be false for any position outside borad" in {
    val board = Board.newGame()

    for (col <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         row <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         pos = Position(col, row)) {
      withClue(s"Checking position: $pos") {
        board.isOccupied(pos) should be(false)
      }
    }
  }

  "Get piece" should "return empty for any position outside board" in {
    val board = Board.newGame()

    for (col <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         row <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         pos = Position(col, row)) {
      withClue(s"Checking position $pos") {
        board.getPiece(pos).isEmpty should be(true)
      }
    }
  }

  it should "return empty for any not occupied position in new board" in {
    val board = Board.newGame()

    for (pos <- Seq(2, 3, 4).flatMap(Board.allPositionsForRow)) {
      withClue(s"Checking position $pos") {
        board.getPiece(pos).isEmpty should be (true)
      }
    }
  }

  it should "return valid piece for all occupied positions on new board" in {
    val board = Board.newGame()

    val allValidPositions = Seq(0, 1, 6, 7).flatMap(Board.allPositionsForRow)
    allValidPositions
        .map(pos => (pos, board.getPiece(pos)))
        .foreach { case (pos, pieceOpt) =>
          val (expectedPieceType, expectedColor) = Board.startPositionMap(pos)
          withClue(s"Checking position $pos ($expectedPieceType, $expectedColor):") {
            pieceOpt.isDefined should be(true)
            pieceOpt.get._1 should be(expectedPieceType)
            pieceOpt.get._2 should be(expectedColor)
          }
        }
  }

  "Move validation" should "succeed for all regular pawn movements on new board" in {
    val board = Board.newGame()

    val oneFieldMoves = Board.allPositionsForRow(1)
                             .map { fromPos =>
                               val piece = board.getPiece(fromPos).get._1
                               assert(piece == Pawn)
                               Move(piece, fromPos, fromPos.move(0, 1), captureAllowed = false)
                             }
    oneFieldMoves.foreach{ move =>
      withClue(s"Checking $move:") {
        board.isValidMove(move) should be(true)
      }
    }
  }
}
