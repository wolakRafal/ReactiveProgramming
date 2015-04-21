package quickcheck

import org.scalacheck._
import Arbitrary._

import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int, b:Int) =>
    val m = if (a < b) a else b
    val h = insert(b,insert(a, empty))
    findMin(h) == m
  }

  property("min3") = forAll { (a:Int, b:Int) =>
    val m = if (a < b) a else b
    val h = meld(insert(a, empty), insert(b,empty))
    findMin(h) == m
  }


  property("min3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { h: H =>
    def toListRec(heap: H, acc: List[Int]): List[Int] = {
      if (isEmpty(heap)) {
        acc
      } else {
        val m = findMin(heap)
        toListRec(deleteMin(heap),  acc ++ List(m))
      }
    }

    val l = toListRec(h, List())
    l == l.sorted
  }


  def toListRec(heap: H, acc: List[Int]): List[Int] = {
    if (isEmpty(heap)) {
      acc
    } else {
      val m = findMin(heap)
      toListRec(deleteMin(heap),  acc ++ List(m))
    }
  }

  property("sorted2") = forAll { (h1: H, h2:H) =>

    val l = toListRec(meld(h1,h2), List())
    l == l.sorted
  }


  property("meld min") = forAll { (h1: H, h2 :H) =>
    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
    val m2 = if(isEmpty(h2)) 0 else findMin(h2)

    val m = Math.min(m1,m2)
    findMin(meld(h1,h2)) == m
  }

  def getAsSortedList(hhh: H, acc: List[A] = List[A]()): List[A] = {
    if (isEmpty(hhh)) acc
    else getAsSortedList(deleteMin(hhh), acc :+ findMin(hhh))
  }

  def checkIsCorrectHeap(heap: H): Boolean = {
    val list = getAsSortedList(heap)// poprawny heap zwroci posortowana liste
    list.sorted == list
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted sequence by continually deleting minium") = forAll {
    h: H => {
      val expectedSortedList = getAsSortedList(h, List[A]())
      //      println("expectedSortedList  " + expectedSortedList )
      expectedSortedList == expectedSortedList.sorted
    }
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other
  property("Finding a minimum of the melding ") = forAll {
    (h1: H, h2: H) =>
      val h1Min = findMin(h1)
      val h2Min = findMin(h2)
      val meldedHeap = meld(h1, h2)
      Math.min(h1Min, h2Min) == findMin(meldedHeap)
  }

  // stworz dwa stosy n- i m- elememtnowy poácz je i usun n+m elementow powinie byc pusty
  // stworz N-eleemntowy stos - usun N i sprawdz czy jest pusty
  property("Empty test - melding ") = forAll {
    (h1: H, h2: H) =>
      val heap1Size = count(h1)
      val heap2Size = count(h2)
      val shouldBeEmpty = deleteNElements(meld(h1,h2), heap1Size + heap2Size)
      isEmpty(shouldBeEmpty)
  }
  property("meld the same") = forAll {
    h1: H =>
      val heap1Size = count(h1)
      count(meld(h1,h1)) == heap1Size * 2
  }
  // usun i saprawdz licznosc
  property("Delete and check size") = forAll {
    h:H =>
      if (!isEmpty(h)) checkIsCorrectHeap(deleteMin(h))
      else true
  }
  property("compare to Heaps") = forAll {
    (h1: H, h2: H) =>
      def compare(heap1: H, heap2: H): Boolean = {
        if(isEmpty(heap1) && isEmpty(heap2)) true
        else {
          val a = findMin(heap1)
          val b = findMin(heap2)
          a == b && compare(deleteMin(heap1),deleteMin(heap2))
        }
      }
      compare(meld(h1,h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }


  def deleteNElements(hhh: H, counter: Int = 0): H = if (counter == 0) hhh else deleteNElements(deleteMin(hhh), counter - 1)

  def count(hhh: H, counter: Int = 0): Int = if (isEmpty(hhh)) counter else count(deleteMin(hhh), counter + 1)



  //  property("meld del min") = forAll { (h1: H, h2 :H) =>
//
//    if (isEmpty(h1) || isEmpty(h2)) {
//      true
//    }
//    else {
//      println(s"h1=$h1 h2=$h2" )
//      val hm1 = deleteMin(h1)
//      println(s"hm1=$hm1 " )
//      val m1 = if (isEmpty(hm1)) {
//        0
//      } else {
//        findMin(hm1)
//      }
//      val hm2 = deleteMin(h2)
//      println(s"hm2=$hm2 " )
//      val m2 = if (isEmpty(hm2)) {
//        0
//      } else {
//        findMin(hm2)
//      }
//
//
//      val m = Math.min(m1, m2)
//      if (!isEmpty(hm1) && !isEmpty(hm2)) {
//      findMin(meld(hm1, hm2)) == m
//    } else true
//  }}


  property("meld with empty heaps") = forAll { h: H =>
    toListRec(meld(h, empty), List()) == toListRec(h, List())
  }

  property("meld empty heaps") = forAll { a: Int =>
    toListRec(insert(a,meld(empty, empty)), List()) == toListRec(empty, List(a))
  }

  lazy val genHeap: Gen[H] = for {
    v <- Gen.posNum[Int]
    h <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(v, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
