package search

import org.scalatest._

import NoRepeats.combinations

class NoRepeatsSpec extends FunSuite {
  val cases = List(
    ("aab", 2),
    ("aaa", 0),
    ("aabb", 8),
    ("abcdefa", 3600),
    ("abfdefa", 2640),
    ("zzzzzzzz", 0),
    ("a", 1),
    ("aaab", 0),
    ("aaabb", 12)
  )

  cases.foreach { c =>
    c match {
      case (str, amount) =>
        test(s"combinations($str) returns $amount") {
          assert(combinations(str.toList) == amount)
        }
    }
  }
}
