package com.mikesajak.chess

class BoardSpec extends UnitTestSpec {
  "isOccupied" should "be true only for occupied only starting positions on new board" in {
    val board = Board.newGame()

    val expectedOccupiedPositions = (allPositionsForRow(0) ++
        allPositionsForRow(1) ++
        allPositionsForRow(6) ++
        allPositionsForRow(7)).toSet

    for (row <- 0 to 7;
         col <- 0 to 7;
         pos = Position(col, row)) {
      withClue(s"Checking position: $pos") {
        if (expectedOccupiedPositions contains pos)
          board.isOccupied(pos) should be(true)
        else
          board.isOccupied(pos) should be(false)
      }
    }
  }

  it should "be false for any position outside borad" in {
    val board = Board.newGame()

    for (col <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         row <- Seq(-100, -5, -2, -1, 8, 10, 1000);
         pos = Position(col, row)) {
      withClue(s"Checking position: $pos")
        board.isOccupied(pos) should be (false)
    }
  }



  private def allPositionsForRow(row: Int) = Seq.tabulate(8)(col => Position(col, row))
}
