package FifthLab

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(n, h))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a < b) a else b)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  def checkSorting(el: Int, heap: H): Boolean = {
    if (isEmpty(heap)) true
    else if (el <= findMin(heap)) checkSorting(findMin(heap), deleteMin(heap))
    else false
  }

  property("sorted_heap_after_deleting") = forAll { h: H =>
    if (isEmpty(h)) true else
    if (isEmpty(deleteMin(h))) true else checkSorting(findMin(h), deleteMin(h))
  }

  property("merging") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min1 = (if (isEmpty(h1)) 0 else findMin(h1))
    val min2 = (if (isEmpty(h2)) 0 else findMin(h2))
    val minH = (if (isEmpty(h)) 0 else findMin(h))
    minH == min1 || minH == min2
  }

  property("adding_two_elements") = forAll { (a: Int, b: Int) =>
    val h = deleteMin(deleteMin(insert(a, insert(b, empty))))
    isEmpty(h)
  }

  property("unchanged_min_element") = forAll { (a: Int, b: Int) =>
    val h = if (a > b) insert(a, insert(b, empty)) else insert(b, insert(a, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("something_with_merging_and_deleting") = forAll { (h1: H, h2: H) =>
    val firstHeap = meld(h1, h2)
    val newH1 = if (isEmpty(h1)) empty else deleteMin(h1)
    val newMergedHeap = if (isEmpty(h1)) meld(h1, h2) else meld(newH1, insert(findMin(h1), h2))
    checkEquality(firstHeap, newMergedHeap)
  }

  def checkEquality(h1: H, h2: H): Boolean = {
    if(isEmpty(h1) || isEmpty(h2)) isEmpty(h1) && isEmpty(h2)
    else {
      if (findMin(h1) == findMin(h2)) checkEquality(deleteMin(h1), deleteMin(h2))
      else false
    }
  }

  property("different_merging") = forAll { (h1: H, h2: H) =>
    val oneWay = meld(h1, h2)
    val anotherWay = meld(h2, h1)
    checkEquality(oneWay, anotherWay)
  }
}
