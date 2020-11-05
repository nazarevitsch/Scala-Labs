package ForthLab

import ForthLab.Anagrams._

object Main {
  def main(args: Array[String]): Unit = {
//    sentenceOccurrences(List("abcd", "e", "Robert"))
//    println(List(('a', 2), ('b', 2)));
//    println(combinations(List(('a', 2), ('b', 2))));
    println(sentenceAnagrams(List("Linux", "rulez")));
  }
}
