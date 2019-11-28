package search

object NoRepeats {
  def combinations (chars: Seq[Char]): Int = {
    val indexedChars = chars zip (0 until chars.size)
    val partials = for {
      (char, index) <- indexedChars
      rest = indexedChars filter (_._2 != index)
    } yield findCombinations((char, index), rest)
    partials.sum
  }

  private def findCombinations(
    current: (Char, Int),
    chars: Seq[(Char, Int)],
    last: Char = ' ',
    total: Int = 0
  ): Int = (current, last, chars) match {
    case ((a, _), b, _) if a == b => total
    case (_, _, Nil)         => total + 1
    case _                   => (for {
      (char, index) <- chars
      rest = chars filter(_._2 != index)
    } yield findCombinations((char, index), rest, current._1, total)).sum
  }
}
