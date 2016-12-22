
val v=Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}


def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = p => {
  try {
    levelVector(p.row)(p.col) != '_'
  } catch {
    case e: IndexOutOfBoundsException => false
  }
}

val level =
  """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin
terrainFunction(v)(Pos(0,-1))
