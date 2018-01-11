object ProteinTranslation {

  def translate(sequence: String): Seq[String] =
    sequence.
      grouped(3).
      map(codons(_)).
      takeWhile(_.isDefined).
      flatten.
      toSeq

  private val codons: Map[String, Option[String]] = Map(
    "AUG" -> Some("Methionine"),
    "UUU" -> Some("Phenylalanine"),
    "UUA" -> Some("Leucine"),
    "UCU" -> Some("Serine"),
    "UAU" -> Some("Tyrosine"),
    "UGU" -> Some("Cysteine"),
    "UGG" -> Some("Tryptophan"),
    "UUC" -> Some("Phenylalanine"),
    "UUG" -> Some("Leucine"),
    "UCC" -> Some("Serine"),
    "UAC" -> Some("Tyrosine"),
    "UGC" -> Some("Cysteine"),
    "UCA" -> Some("Serine"),
    "UCG" -> Some("Serine"),
    "UAA" -> None,
    "UAG" -> None,
    "UGA" -> None
  )
}
