object Raindrops {

  private val factors = Seq(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")

  def convert(n: Int): String =
    factors.collect({ case (i, word) if n % i == 0 => word }).mkString match {
      case "" => n.toString
      case s  => s
    }
}
