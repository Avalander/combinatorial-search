package search
import scala.annotation.tailrec

object Pairwise {
  def pairwise (xs: Seq[Int], target: Int): Int = {
    val indexed = xs zip (0 until xs.size)
    xs match {
      case Nil => 0
      case _   => indexedPairwise(indexed, target)
    }
  }

  @tailrec
  private def indexedPairwise (xs: Seq[(Int, Int)], target: Int, total: Int = 0): Int = {
    val ((value, index) :: rest) = xs
    if (rest.isEmpty) total
    else {
      val second = rest find {
        case (value_y, _) => value + value_y == target
      }
      second match {
        case None => indexedPairwise(rest, target, total)
        case Some((_, index_y)) => {
          val next = rest filter {
            case (_, index) => index != index_y
          }
          val newTotal = total + index + index_y
          next match {
            case Nil => newTotal
            case _   => indexedPairwise(next, target, newTotal)
          }
        }
      }
    }
  }
}
