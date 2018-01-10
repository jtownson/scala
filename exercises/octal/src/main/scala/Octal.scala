import scala.annotation.tailrec

object Octal {

  def intToOctal(i: Int): String = {

    def qr(n: Int, b: Int): (Int, Int) = (n / b, n % b)

    @tailrec
    def loop(n: Int, acc: String): String = {
      if (n == 0)
        if (acc.isEmpty)
          "0"
        else
          acc
      else {
        val (quotient, remainder) = qr(n, 8)
        loop(quotient, remainder.toString + acc)
      }
    }

    loop(i, "")
  }

  def octalToInt(s: String): Int = base2Int(s.trim, 8, 1)

  private def base2Int(s: String, base: Int, multiplier: Int): Int = {

    val maxResult = Int.MaxValue / base

    // TBH, I would have done the fold, like your example,
    // but didn't want it to look like I was copy and pasting:-)
    @tailrec
    def loop(result: Int, i: Int): Int = {
      if (i < s.length) {
        val digit = Character.digit(s.charAt(i), base)
        if (digit == -1)
          throw new IllegalArgumentException(s"The character '$digit' is not valid for base $base")

        if (result > maxResult)
          throw new IllegalArgumentException(s"The number $s is too big.")

        loop(result * base + digit, i+1)
      } else {
        result
      }
    }

    if (s.isEmpty)
      throw new IllegalArgumentException("Empty string does not represent a number.")

    if (s.charAt(0) == '+')
      base2Int(s.tail, base, multiplier)
    else if (s.charAt(0) == '-')
      base2Int(s.tail, base, -multiplier)
    else
      multiplier * loop(0, 0)

  }
}
