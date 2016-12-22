package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /** If you insert any two elements into an empty heap, finding the minimum of the
    * resulting heap should get the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  /** If you insert an element into an empty heap, then delete the minimum,
    * the resulting heap should be empty.
    *
    */

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when
    * continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
    */

  property("findDeleteMinima") = forAll { (h: H) =>
    def isSorted(h: H, l: List[A]): Boolean = {
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        if (!l.isEmpty && l.max > min) false
        else isSorted(deleteMin(h), min :: l)
      }
    }
    isSorted(h, List())
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */

  property("minMeld") = forAll { (h1: H, h2: H) =>

    min(findMin(h1), findMin(h2)) == findMin(meld(h1, h2))

  }

  /**
    * check if melding two heaps equal to melding of minima deleted from one heap and
    * inserting it in the other
    */

  property("meldDelete") = forAll { (h1: H, h2: H) =>

    def isEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        (findMin(h1) == findMin(h2)) && (isEqual(deleteMin(h1), deleteMin(h2)))
      }
    }

    val m1 = meld(h1, h2)
    val m2 = meld(deleteMin(h1), insert(findMin(h1), h2))

    isEqual(m1, m2)
  }


}
