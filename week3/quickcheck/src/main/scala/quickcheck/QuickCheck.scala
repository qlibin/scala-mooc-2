package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genNonEmptyHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- frequency((1, const(empty)), (9, genNonEmptyHeap))
  } yield insert(x, h)

  lazy val genHeap: Gen[H] = for {
    h <- frequency((1, const(empty)), (9, genNonEmptyHeap))
  } yield h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def deleteAll(rest: H, deleted: List[A]): List[A] = {
    if (isEmpty(rest))
      deleted
    else {
      val min = findMin(rest)
      deleteAll(deleteMin(rest), min :: deleted)
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin of a inserted to empty should be a") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin of a and b inserted to empty should be min(a, b)") = forAll { (a: Int, b: Int) =>
    val ha = insert(a, empty)
    val hab = insert(b, ha)
    findMin(hab) == Seq(a, b).min
  }

  property("check deletion from 1 element heap") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("check deletion order") = forAll { (h: H) =>
    val allDeleted = deleteAll(h, Nil)
    allDeleted.reverse == allDeleted.sorted
  }

  property("check deletion order of melded") = forAll { (a: H, b: H) =>
    val mina = if (isEmpty(a)) Int.MaxValue else findMin(a)
    val minb = if (isEmpty(b)) Int.MaxValue else findMin(b)
    val melded = meld(a, b)
    val allDeleted = deleteAll(melded, Nil)
    allDeleted.reverse == allDeleted.sorted
  }

  property("findMin of melded should be correct") = forAll { (a: H, b: H) =>
    val mina = if (isEmpty(a)) Int.MaxValue else findMin(a)
    val minb = if (isEmpty(b)) Int.MaxValue else findMin(b)
    val melded = meld(a, b)
    Seq(mina, minb).min == (if (isEmpty(melded)) Int.MaxValue else findMin(melded))
  }

//  property("findMin of 2 melded should be correct") = forAll { (a: H, b: H) =>
//    val mina = if (isEmpty(a)) Int.MaxValue else findMin(a)
//    val minb = if (isEmpty(b)) Int.MaxValue else findMin(b)
//    // insert min value possible
//    val melded = meld(insert(Int.MinValue, empty), meld(a, b))
//    // delete min valiue possible
//    val melded_deleted = deleteMin(melded)
//    Seq(mina, minb).min == (if (isEmpty(melded_deleted)) Int.MaxValue else findMin(melded_deleted))
//  }

//  property("findMin of melded (with args replaced) should be correct") = forAll { (a: H, b: H) =>
//    val mina = if (isEmpty(a)) Int.MaxValue else findMin(a)
//    val minb = if (isEmpty(b)) Int.MaxValue else findMin(b)
//    val melded = meld(b, a)
//    Seq(mina, minb).min == (if (isEmpty(melded)) Int.MaxValue else findMin(melded))
//  }
//
//  property("min of meld of h with empty should be correct") = forAll { (h: H) =>
//    val minh = if (isEmpty(h)) None else Some(findMin(h))
//    val melded = meld(h, empty)
//    minh match {
//      case None => melded == empty
//      case Some(min) => findMin(melded) == min
//    }
//  }

//  property("findMin of 2 melded should be correct") = forAll { (a: H, b: H) =>
//    val mina = if (isEmpty(a)) Int.MaxValue else findMin(a)
//    val minb = if (isEmpty(b)) Int.MaxValue else findMin(b)
//    // insert min value possible
//    val melded = meld(insert(Int.MinValue, empty), meld(a, b))
//    // delete min valiue possible
//    val melded_deleted = deleteMin(melded)
//    Seq(mina, minb).min == (if (isEmpty(melded_deleted)) Int.MaxValue else findMin(melded_deleted))
//  }

  property("check deletion order") = forAll(Gen.choose(1, 100)) { (n: Int) =>
    def insertAll(numbers: List[Int], acc: H): H = numbers match {
      case Nil => acc
      case x :: xs => insertAll(xs, insert(x, acc))
    }
    val range = (0 to n).toList
    val all = insertAll(range, empty)
    findMin(all) == 0
    val allDeleted = deleteAll(all, Nil)
    allDeleted.reverse == range
  }

}
