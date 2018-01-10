object BeerSong {

  def verse(i: Int): String = i match {
    case 0 =>
      """No more bottles of beer on the wall, no more bottles of beer.
        |Go to the store and buy some more, 99 bottles of beer on the wall.
        |""".stripMargin

    case 1 =>
      """1 bottle of beer on the wall, 1 bottle of beer.
        |Take it down and pass it around, no more bottles of beer on the wall.
        |""".stripMargin

    case 2 =>
      """2 bottles of beer on the wall, 2 bottles of beer.
        |Take one down and pass it around, 1 bottle of beer on the wall.
        |""".stripMargin

    case _ if i < 100 =>
      s"""$i bottles of beer on the wall, $i bottles of beer.
        |Take one down and pass it around, ${i-1} bottles of beer on the wall.
        |""".stripMargin
  }

  def verses(i0: Int, i1: Int): String = {

    val step = if (i1 >= i0) 1 else -1
    val range = Range.inclusive(i0, i1, step)

    (range map verse).mkString("\n")
  }
}
