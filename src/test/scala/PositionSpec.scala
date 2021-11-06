package com.mikesajak.chess

class PositionSpec extends UnitTestSpec {
  "Position constructor" should "set row and column" in {
    val pos = Position(1, 2)
    pos.col should be (1)
    pos.row should be (2)
  }

  "Position move" should "update row according to move direction" in {
    val pos = Position(3, 4)
    for (rowOfs <- -5 to 10;
         colOfs <- -5 to 10;
         newPos = pos.move(colOfs, rowOfs)) {
      withClue(s"${positionsInfo(pos, newPos)}, col diff") {
        newPos.col should be(pos.col + colOfs)
      }
      withClue(s"${positionsInfo(pos, newPos)}, row diff") {
        newPos.row should be(pos.row + rowOfs)
      }
    }
  }

  "Position row and col diff" should "calculate vertical and horizontal difference" in {
    val pos1 = Position(5, 10)
    for (col2 <- -15 to 15;
         row2 <- -15 to 15;
         pos2 = Position(col2, row2)) {
      withClue(s"${positionsInfo(pos1, pos2)}, col diff") {
        pos1.colDiff(pos2) should be(col2 - pos1.col)
      }
      withClue(s"${positionsInfo(pos1, pos2)}, row diff") {
        pos1.rowDiff(pos2) should be(row2 - pos1.row)
      }
    }
  }

  it should "be invertible" in {
    val pos1 = Position(10, 10)
    val pos2 = Position(20, 20)

    pos1.colDiff(pos2) should be (-pos2.colDiff(pos1))
    pos1.rowDiff(pos2) should be (-pos2.rowDiff(pos1))
  }

  private def positionsInfo(pos1: Position, pos2: Position) =
    s"(pos1: $pos1 (col=${pos1.col}, row=${pos1.row}), pos2: $pos2 (col=${pos2.col}, row=${pos2.row}))"
}
