import fastparse.all._

// SGF4 EBNF grammar for reference at http://www.red-bean.com/sgf/sgf4.html
case class Node[A](rootLabel: A, subForest: Seq[Node[A]] = List())

object Sgf {

  type SgfTree = Node[SgfNode]

  type SgfNode = Map[String, Seq[String]]

  // GameTree   ::= "(" Sequence { GameTree } ")"
  // Here replaced by a simpler construct with a single node at the head rather than a sequence
  def SgfTree: P[SgfTree] = P("(" ~ Sequence ~ SgfTree.rep ~ ")").
    map((s: (SgfTree, Seq[SgfTree])) => Node(s._1.rootLabel, s._1.subForest ++ s._2))

  // Sequence   ::= Node { Node }
  // Here shoehorned into Node
  def Sequence: P[SgfTree] = P(SgfNode ~ SgfNode.rep(min = 0)).map({case (n0, ns: Seq[SgfNode]) => Node(n0, ns.map(Node(_)))})

  // Node       ::= ";" { Property }
  def SgfNode: P[SgfNode] = P(";" ~ Property.rep).map(_.toMap)

  // Property   ::= PropIdent PropValue { PropValue }
  def Property: P[(String, Seq[String])] = P(PropIdent ~ PropValue.rep(min = 1)).map({case (k, v) => (k, v)})

  // PropIdent  ::= UcLetter { UcLetter }
  def PropIdent: P[String] = P(CharIn('A' to 'Z').rep.!)

  // PropValue  ::= "[" CValueType "]"
  def PropValue: P[String] = "[" ~ SimpleText ~ "]"

  // Text       ::= { any character }
  def SimpleText: P[String] = P( (Whitespace | EscapedNewline | EscapedChar | AlphaNum).rep(1).! ).map(s =>
    // Unfortunately fastparse's map function does not seem to work in the presence of the or (|) combinator.
    // Necessitates this slightly ugly workaround
    s.replace("\\\n", "")
      .replace("\t", " ")
      .replace("\n", " ")
      .replaceAll("""\\(.)""", "$1"))

  def AlphaNum: Parser[String] = P( (CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9')).! )

  def Whitespace: Parser[String] = P( ("\t" | "\n" | " ").! )

  def EscapedNewline: Parser[String] = P( ("""\""" ~ "\n").map(_ => "") )

  def EscapedChar: P[String] = P("""\""" ~ AnyChar.!)

  def parseSgf(text: String): Option[SgfTree] =
    SgfTree.parse(text).fold((_, _, _) => None, (sgfTree, _) => Option(sgfTree))
}
