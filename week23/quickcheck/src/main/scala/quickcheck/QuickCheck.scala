package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
    )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert 2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val smallest = if (a > b) b else a
    findMin(h) == smallest
  }

  property("min3") = forAll { h: H =>
    if (collect(h).length < 3) true
    else {
      val m1 = findMin(h)
      val m2 = findMin(deleteMin(h))
      m2 >= m1
      val m3 = findMin(deleteMin(deleteMin(h)))
      m3 >= m2
    }
  }

  property("delete min") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def collect(h: H, l: List[A] = List()): List[A] =
    if (isEmpty(h)) l.reverse
    else collect(deleteMin(h), findMin(h) :: l)

  property("sorted delete") = forAll { heap: H =>
    val collected = collect(heap)
    collected == collected.sorted
  }

  property("meld min") = forAll { (h1: H, h2: H) =>
    (isEmpty(h1), isEmpty(h2)) match {
      case (false, false) => {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        val meldedMin = findMin(meld(h1, h2))

        (meldedMin == min1) || (meldedMin == min2)
      }
      case (true, false) => findMin(h2) == findMin(meld(h1, h2))

      case (false, true) => findMin(h1) == findMin(meld(h1, h2))

      case (true, true) => true
    }
  }

  property("meld collected") = forAll { (h1: H, h2: H) =>
    val collected1 = collect(h1)
    val collected2 = collect(h2)
    val melded = meld(h1, h2)
    val collectedMeld = collect(melded)
    collectedMeld.sorted == (collected1 ++ collected2).sorted
  }

}
