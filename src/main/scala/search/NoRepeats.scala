package search

object NoRepeats {
  def combinations (chars: Seq[Char]): Int = {
    val indexedChars = chars zip (0 until chars.size)
    findPartials(indexedChars)
    // val partials = for {
    //   (char, index) <- indexedChars
    //   rest = indexedChars filter (_._2 != index)
    // } yield findCombinations(char, rest)
    // partials.sum
  }

  private def findCombinations(
    current: Char,
    chars: Seq[(Char, Int)],
    last: Char,
    total: Int
  ): Int = (current, last, chars) match {
    case (a, b, _) if a == b => total
    case (_, _, Nil)         => total + 1
    case _                   => findPartials(chars, current, total)
    // case _                   => (for {
    //   (char, index) <- chars
    //   rest = chars filter(_._2 != index)
    // } yield findCombinations(char, rest, current, total)).sum
  }

  private def findPartials (chars: Seq[(Char, Int)], last: Char = ' ', total: Int = 0): Int =
    (for {
      (char, index) <- chars
      rest = chars filter(_._2 != index)
    } yield findCombinations(char, rest, last, total)).sum
}
