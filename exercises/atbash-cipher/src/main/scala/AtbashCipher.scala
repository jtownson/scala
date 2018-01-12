import scala.collection.mutable.StringBuilder.newBuilder

object AtbashCipher {

  val key = Map(
    'a' -> 'z',
    'b' -> 'y',
    'c' -> 'x',
    'd' -> 'w',
    'e' -> 'v',
    'f' -> 'u',
    'g' -> 't',
    'h' -> 's',
    'i' -> 'r',
    'j' -> 'q',
    'k' -> 'p',
    'l' -> 'o',
    'm' -> 'n',
    'n' -> 'm',
    'o' -> 'l',
    'p' -> 'k',
    'q' -> 'j',
    'r' -> 'i',
    's' -> 'h',
    't' -> 'g',
    'u' -> 'f',
    'v' -> 'e',
    'w' -> 'd',
    'x' -> 'c',
    'y' -> 'b',
    'z' -> 'a'
  )

  val tr: (StringBuilder, Char) => StringBuilder = (sb, c) =>
    if (c.isLetter)
      sb.append(key(c.toLower))
    else if (c.isDigit)
      sb.append(c)
    else
      sb

  def decode(s: String): String = s.foldLeft(newBuilder)(tr).toString()

  def encode(s: String): String = s.foldLeft(newBuilder)(tr).grouped(5).mkString(" ")

}
