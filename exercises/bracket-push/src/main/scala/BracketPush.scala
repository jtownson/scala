import scala.util.Try

object BracketPush {

  private val opensExpression: Char => Boolean = Set('(', '[', '{')

  private val mayCloseExpression: Char => Boolean = Set(')', ']', '}')

  private val doesCloseExpression: (Char, Char) => Boolean = Function.untupled(Map(
    ('(', ')') -> true,
    ('[', ']') -> true,
    ('{', '}') -> true
  ).withDefaultValue(false))

  private val matchBrackets: (List[Char], Char) => List[Char] = (list, c) => {
    if (opensExpression(c))
      c :: list
    else if (mayCloseExpression(c))
      if (list.nonEmpty && doesCloseExpression(list.head, c))
        list.tail
      else
        throw new IllegalArgumentException(s"Syntax error. Unmatched closing character '$c'")
    else
      list
  }

  def isPaired(s: String): Boolean =
    Try(s.foldLeft[List[Char]](Nil)(matchBrackets).isEmpty).getOrElse(false)
}
