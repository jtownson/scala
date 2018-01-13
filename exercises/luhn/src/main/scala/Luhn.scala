object Luhn {

  def valid(s: String): Boolean = {
    val ss = s.replace(" ", "").reverse

    ss.length > 1 &&
    ss.forall(_.isDigit) &&
    valid(ss.map(_.asDigit))
  }


  private def valid(s: IndexedSeq[Int]): Boolean = {
    s.indices.collect({
      case i if isOdd(i) => doubleAndSubtract(s(i))
      case i => s(i)
    }).sum % 10 == 0
  }

  private def isOdd(i: Int): Boolean = i % 2 == 1

  private def doubleAndSubtract(i: Int): Int = {
    val doubled =  i * 2
    if (doubled > 9)
      doubled - 9
    else
      doubled
  }
}
