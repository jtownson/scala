import java.lang.Character._

import scala.collection.mutable.StringBuilder.newBuilder

object RunLengthEncoding {

  type RunLength = StringBuilder
  type Decoded = StringBuilder

  def decode(s: String): String = {

    val result: (RunLength, Decoded) =
      s.foldLeft((newBuilder, newBuilder)) {
        case ((rl, dec), c) =>

          if (isEncoded(c) && rl.nonEmpty)
            (newBuilder, dec.append(c.toString * rl.toString().toInt))

          else if (isEncoded(c) && rl.isEmpty)
            (rl, dec.append(c.toString * 1))

          else if (isDigit(c))
            (rl.append(c), dec)

          else
            (rl, dec.append(c))
      }
    result._2.toString()
  }

  private def isEncoded(c: Char): Boolean =
    isLetter(c) || isWhitespace(c)

  def encode(s: String): String =
    encode(s, newBuilder)

  private def encode(s: String, sb: StringBuilder): String = {
    if (s.isEmpty)
      sb.toString()
    else {
      val (front, back) = s.span(c => c == s.head)
      encode(back, sb.append(size2Str(front)).append(front.head))
    }
  }

  private def size2Str(s: String): String = {
    val length = s.length
    if (length == 1)
      ""
    else
      length.toString
  }
}
