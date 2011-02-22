package de.sciss.fingertree

/* HH import scalaz.Scalaz._ */
// HH
import Helper._

import collection.Iterator
import collection.immutable.StringLike
import annotation.tailrec

/**
* Finger Trees provide a base for implementations of various collection types,
* as described in "Finger trees: a simple general-purpose data structure", by
* Ralf Hinze and Ross Paterson.
* <br>
* Finger Trees have excellent (amortized) asymptotics:
* <br>
* Access to the first and last elements is O(1).
* Appending/prepending a single value is O(1).
* Concatenating two trees is (O lg min(l1, l2)) where l1 and l2 are their sizes.
* Random access to an element at n is O(lg min(n, l - n)), where
*   l is the size of the tree.
* Constructing a tree with n copies of a value is O(lg n).
**/

sealed abstract class ViewL[S[_], A] {
  def fold[B](b: => B, f: (=> A, => S[A]) => B): B
  def headOption: Option[A] = fold(None, (a, sa) => Some(a))
  def tailOption: Option[S[A]] = fold(None, (a, sa) => Some(sa))
  def head: A = headOption.getOrElse(error("Head on empty view"))
  def tail: S[A] = tailOption.getOrElse(error("Tail on empty view"))
}

sealed abstract class ViewR[S[_], A] {
  def fold[B](b: => B, f: (=> S[A], => A) => B): B
  def lastOption: Option[A] = fold(None, (sa, a) => Some(a))
  def initOption: Option[S[A]] = fold(None, (sa, a) => Some(sa))
  def last: A = lastOption.getOrElse(error("Last on empty view"))
  def init: S[A] = initOption.getOrElse(error("Init on empty view"))
}

import FingerTree._

// aka Digit in H+P
sealed trait /* HH abstract class */ Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  def +:(a: => A): Finger[V, A]

  def :+(a: => A): Finger[V, A]

  def |-:(a: => A): Finger[V, A]

  def :-|(a: => A): Finger[V, A]

  def lhead: A

  def ltail: Finger[V, A]

  def rhead: A

  def rtail: Finger[V, A]

  def toTree: FingerTree[V, A]

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): Finger[V2, B]

  def foreach(f: A => Unit): Unit

  def iterator: Iterator[A]

  def reverseIterator: Iterator[A]

  def measure: V

  def toList = map(x => x)(ListReducer[A]).measure

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]])
}
case class One[V, A](v: V, a1: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)

  def +:(a: => A) = Two(a cons v, a, a1)

  def :+(a: => A) = Two(v snoc a, a1, a)

  def |-:(a: => A) = one(a)

  def :-|(a: => A) = one(a)

  def lhead = a1

  def ltail = error("Tail on the digit One")

  def rhead = a1

  def rtail = error("Tail on the digit One")

  def toTree = single(a1)

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = one(f(a1))

  def foreach(f: A => Unit) {
    f(a1)
  }

  def iterator = Iterator.single(a1)

  def reverseIterator = Iterator.single(a1)

  val measure = v

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V) = (None, a1, None)
}
case class Two[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2)

  def +:(a: => A) = Three(a cons v, a, a1, a2)

  def :+(a: => A) = Three(v snoc a, a1, a2, a)

  def |-:(a: => A) = two(a, a2)

  def :-|(a: => A) = two(a1, a)

  def lhead = a1

  def ltail = one(a2)

  def rhead = a2

  def rtail = one(a1)

  def toTree = {
    deep(v, one(a1), empty[V, Node[V, A]], one(a2))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = two(f(a1), f(a2))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
  }

  def iterator = Iterator(a1, a2)

  def reverseIterator = Iterator(a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(one(a2)))
    else
      (Some(One(va1, a1)), a2, None)
  }
}
case class Three[V, A](v: V, a1: A, a2: A, a3: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3)

  def +:(a: => A) = Four(a cons v, a, a1, a2, a3)

  def :+(a: => A) = Four(v snoc a, a1, a2, a3, a)

  def |-:(a: => A) = three(a, a2, a3)

  def :-|(a: => A) = three(a1, a2, a)

  def lhead = a1

  def ltail = two(a2, a3)

  def rhead = a3

  def rtail = two(a1, a2)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], one(a3))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = three(f(a1), f(a2), f(a3))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
    f(a3)
  }

  def iterator = Iterator(a1, a2, a3)

  def reverseIterator = Iterator(a3, a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(two(a2, a3)))
    else {
      val accVa2 = accVa1 snoc a2
      if (pred(accVa2))
        (Some(One(va1, a1)), a2, Some(one(a3)))
      else
        (Some(two(a1, a2)), a3, None)
    }
  }
}
case class Four[V, A](v: V, a1: A, a2: A, a3: A, a4: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3) |+| f(a4)

  def +:(a: => A) = error("Digit overflow")

  def :+(a: => A) = error("Digit overflow")

  def |-:(a: => A) = four(a, a2, a3, a4)

  def :-|(a: => A) = four(a1, a2, a3, a)

  def lhead = a1

  def ltail = three(a2, a3, a4)

  def rhead = a4

  def rtail = three(a1, a2, a3)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], two(a3, a4))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = four(f(a1), f(a2), f(a3), f(a4))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
    f(a3)
    f(a4)
  }

  def iterator = Iterator(a1, a2, a3, a4)

  def reverseIterator = Iterator(a4, a3, a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(three(a2, a3, a4)))
    else {
      val accVa2 = accVa1 snoc a2
      if (pred(accVa2))
        (Some(One(va1, a1)), a2, Some(two(a3, a4)))
      else {
        val accVa3 = accVa2 snoc a3
        if (pred(accVa3))
          (Some(two(a1, a2)), a3, Some(one(a4)))
        else
          (Some(three(a1, a2, a3)), a4, None)
      }
    }
  }
}

sealed abstract class Node[V, A](implicit r: Reducer[A, V]) {
  def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B): B

  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B = fold(
    (v, a1, a2) => f(a1) |+| f(a2),
    (v, a1, a2, a3) => f(a1) |+| f(a2) |+| f(a3))

  def toDigit = fold(
    (v, a1, a2) => Two(v, a1, a2),
    (v, a1, a2, a3) => Three(v, a1, a2, a3))

  val measure: V

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]) = fold(
    (v, a1, a2) => node2(f(a1), f(a2)),
    (v, a1, a2, a3) => node3(f(a1), f(a2), f(a3)))

  def foreach(f: A => Unit) {
    fold(
      (_, a1, a2) => { f(a1); f(a2) },
      (_, a1, a2, a3) => { f(a1); f(a2); f(a3) }
    )}

  def iterator = fold(
    (_, a1, a2) => Iterator(a1, a2),
    (_, a1, a2, a3) => Iterator(a1, a2, a3))

  def reverseIterator = fold(
    (_, a1, a2) => Iterator(a2, a1),
    (_, a1, a2, a3) => Iterator(a3, a2, a1))

  private implicit def sg: Semigroup[V] = r.monoid

  /* HH private[scalaz] */ def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]]) = fold(
    (v, a1, a2) => {
      val va1 = r.unit(a1)
      val accVa1 = accV |+| va1
      if (pred(accVa1))
        (None, a1, Some(one(a2)))
      else
        (Some(One(va1, a1)), a2, None)
    },
    (v, a1, a2, a3) => {
      val va1 = r.unit(a1)
      val accVa1 = accV |+| va1
      if (pred(accVa1))
        (None, a1, Some(two(a2, a3)))
      else {
        val accVa2 = accVa1 snoc a2
        if (pred(accVa2))
          (Some(One(va1, a1)), a2, Some(one(a3)))
        else
          (Some(two(a1, a2)), a3, None)
      }
    })
}

sealed abstract class FingerTree[V, A](implicit measurer: Reducer[A, V]) {
  def measure = this.unit[V]

  def foldMap[B](f: A => B)(implicit s: Monoid[B]): B =
    fold(v => s.zero, (v, x) => f(x), (v, pr, m, sf) => pr.foldMap(f) |+| m.foldMap(x => x.foldMap(f)) |+| sf.foldMap(f))

  def fold[B](empty: V => B, single: (V, A) => B, deep: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B

  def +:(a: => A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(v => single(a cons v, a), (v, b) => deep(a cons v, one(a), empty[V, Node[V, A]], one(b)), (v, pr, m, sf) => {
      val mz = m
      pr match {
        case Four(vf, b, c, d, e) => deep(a cons v, two(a, b), node3(c, d, e) +: mz, sf)
        case _ => deep(a cons v, a +: pr, mz, sf)
      }})
  }

  def :+(a: => A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(v => single(v snoc a, a), (v, b) => deep(v snoc a, one(b), empty[V, Node[V, A]], one(a)), (v, pr, m, sf) => {
      val mz = m
      sf match {
        case Four(vf, b, c, d, e) => deep(v snoc a, pr, (mz :+ node3(b, c, d)), two(e, a))
        case _ => deep(v snoc a, pr, mz, sf :+ a)
      }})
  }

  def |-:(a: => A): FingerTree[V, A] = {
    fold(
      v => error("Replacing first element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(a |-: pr, m, sf))
  }

  def :-|(a: => A): FingerTree[V, A] = {
    fold(
      v => error("Replacing last element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(pr, m, sf :-| a))
  }

  def <++>(right: FingerTree[V, A]): FingerTree[V, A] = fold(
      v => right,
      (v, x) => x +: right,
      (v1, pr1, m1, sf1) =>
        right.fold(
          v => this,
          (v, x) => this :+ x,
          (v2, pr2, m2, sf2) => deep(v1 |+| v2, pr1, addDigits0(m1, sf1, pr2, m2), sf2)
        )
    )

  private type ATree = FingerTree[V, A]
  private type AFinger = Finger[V, A]
  private type NodeTree = FingerTree[V, Node[V, A]]

  private implicit def sg: Monoid[V] = measurer.monoid

  def add1(n: A, right: => ATree): ATree = fold(
    v => n +: right,
    (v, x) => x +: n +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n,
        (v, x) => this :+ n :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n) |+| v2, pr1, addDigits1(m1, sf1, n, pr2, m2), sf2)
      )
  )

  def add2(n1: => A, n2: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: right,
    (v, x) => x +: n1 +: n2 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n1 :+ n2,
        (v, x) => this :+ n1 :+ n2 :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n1 snoc n2) |+| v2, pr1, addDigits2(m1, sf1, n1, n2, pr2, m2), sf2)
      )
  )

  def add3(n1: => A, n2: => A, n3: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: n3 +: right,
    (v, x) => x +: n1 +: n2 +: n3 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
         v => this :+ n1 :+ n2 :+ n3,
         (v, x) => this :+ n1 :+ n2 :+ n3 :+ x,
         (v2, pr2, m2, sf2) =>
           deep((v1 snoc n1 snoc n2 snoc n3) |+| v2,
             pr1, addDigits3(m1, sf1, n1, n2, n3, pr2, m2), sf2)
      )
  )

  def add4(n1: => A, n2: => A, n3: => A, n4: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: n3 +: n4 +: right,
    (v, x) => x +: n1 +: n2 +: n3 +: n4 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n1 :+ n2 :+ n3 :+ n4,
        (v, x) => this :+ n1 :+ n2 :+ n3 :+ n4 :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n1 snoc n2 snoc n3 snoc n4) |+| v2,
            pr1, addDigits4(m1, sf1, n1, n2, n3, n4, pr2, m2), sf2)
      )
  )

  def addDigits0(m1: => NodeTree, dig1: => AFinger, dig2: => AFinger, m2: => NodeTree): NodeTree = dig1 match {
    case One(_, a) => dig2 match {
      case One(_, b) => m1.add1(node2(a, b), m2)
      case Two(_, b,c) => m1.add1(node3(a,b,c), m2)
      case Three(_, b,c,d) => m1.add2(node2(a,b), node2(c,d),m2)
      case Four(_, b,c,d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => dig2 match {
      case One(_, c) => m1.add1(node3(a,b,c), m2)
      case Two(_, c,d) => m1.add2(node2(a,b), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
    }
    case Three(_, a,b,c) => dig2 match {
      case One(_, d) => m1.add2(node2(a,b), node2(c,d), m2)
      case Two(_, d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node2(d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => dig2 match {
      case One(_, e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Two(_, e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node2(d,e), node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add3(node3(a,b,c), node3(d,e,f), node2(g,h), m2)
    }
  }

  def addDigits1(m1: => NodeTree, d1: => AFinger, x: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add1(node3(a,x,b), m2)
      case Two(_, b,c) => m1.add2(node2(a,x), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add2(node3(a,x,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add2(node3(a,x,b), node3(c,d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node2(a,b), node2(x,c), m2)
      case Two(_, c,d) => m1.add2(node3(a,b,x), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add2(node3(a,b,x), node3(c,d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node2(c,d), node2(e,f), m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add2(node3(a,b,c), node2(x,d), m2)
      case Two(_, d,e) => m1.add2(node3(a,b,c), node3(x,d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node2(x,d), node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node3(x,d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add2(node3(a,b,c), node3(d,x,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node2(d,x), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node3(d,x,e), node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add3(node3(a,b,c), node3(d,x,e), node3(f,g,h), m2)
    }
  }

  def addDigits2(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node2(a,x), node2(y,b), m2)
      case Two(_, b,c) => m1.add2(node3(a,x,y), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add2(node3(a,x,y), node3(b,c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node2(b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node3(a,b,x), node2(y,c), m2)
      case Two(_, c,d) => m1.add2(node3(a,b,x), node3(y,c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node2(y,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node3(y,c,d), node2(e,f), m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add2(node3(a,b,c), node3(x,y,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node2(x,y), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node3(x,y,d), node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node3(x,y,d), node3(e,f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node2(d,x), node2(y,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node3(d,x,y), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node3(d,x,y), node3(e,f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node2(e,f), node2(g,h), m2)
    }
  }

  def addDigits3(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, z: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node3(a,x,y), node2(z,b), m2)
      case Two(_, b,c) => m1.add2(node3(a,x,y), node3(z,b,c), m2)
      case Three(_, b,c,d) => m1.add3(node3(a,x,y), node2(z,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node3(z,b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node3(a,b,x), node3(y,z,c), m2)
      case Two(_, c,d) => m1.add3(node3(a,b,x), node2(y,z), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node3(y,z,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node3(y,z,c), node3(d,e,f),m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add3(node3(a,b,c), node2(x,y), node2(z,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node3(x,y,z), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node3(x,y,z), node3(d,e,f), m2)
      case Four(_, d,e,f,g) => m1.add4(node3(a,b,c), node3(x,y,z), node2(d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node3(d,x,y), node2(z,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node3(d,x,y), node3(z,e,f), m2)
      case Three(_, e,f,g) => m1.add4(node3(a,b,c), node3(d,x,y), node2(z,e),node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,e,f), node2(g,h), m2)
    }
  }

  def addDigits4(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, z: => A, w: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node3(a,x,y), node3(z,w,b), m2)
      case Two(_, b,c) => m1.add3(node3(a,x,y), node2(z,w), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add3(node3(a,x,y), node3(z,w,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node3(z,w,b), node3(c,d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add3(node3(a,b,x), node2(y,z), node2(w,c), m2)
      case Two(_, c,d) => m1.add3(node3(a,b,x), node3(y,z,w), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node3(y,z,w), node3(c,d,e), m2)
      case Four(_, c,d,e,f) => m1.add4(node3(a,b,x), node3(y,z,w), node2(c,d), node2(e,f),m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add3(node3(a,b,c), node3(x,y,z), node2(w,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node3(x,y,z), node3(w,d,e), m2)
      case Three(_, d,e,f) => m1.add4(node3(a,b,c), node3(x,y,z), node2(w,d),node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add4(node3(a,b,c), node3(x,y,z), node3(w,d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node3(d,x,y), node3(z,w,e), m2)
      case Two(_, e,f) => m1.add4(node3(a,b,c), node3(d,x,y), node2(z,w), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,w,e),node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,w,e), node3(f,g,h), m2)
    }
  }

  def split(pred: V => Boolean): (FingerTree[V, A], FingerTree[V, A]) =
    if (!isEmpty && pred(measure)) {
      val (l, x, r) = split1(pred)
      (l, x +: r)
    }
    else
      (this, empty)

   def takeUntil( p: V => Boolean ) : FingerTree[ V, A ] = split( p )._1
   def dropUntil( p: V => Boolean ) : FingerTree[ V, A ] = split( p )._2

  def split1(pred: V => Boolean): (FingerTree[V, A], A, FingerTree[V, A]) = split1(pred, measurer.monoid.zero)

  private def split1(pred: V => Boolean, accV: V): (FingerTree[V, A], A, FingerTree[V, A]) = fold(
    v => error("Splitting an empty FingerTree"), // we can never get here
    (v, x) => (empty, x, empty),
    (v, pr, m, sf) => {
      val accVpr = accV snoc pr
      if (pred(accVpr)) {
        val (l, x, r) = pr.split1(pred, accV)
        (l.map( _.toTree ).getOrElse( empty ), x, deepL(r, m, sf))
      }
      else {
        val accVm = mappendVal(accVpr, m)
        if (pred(accVm)) {
          val (ml, xs, mr) = m.split1(pred, accVpr)
          val (l, x, r) = xs.split1(pred, mappendVal(accVpr, ml))
          (deepR(pr, ml, l), x, deepL(r, mr, sf))
        }
        else {
          val (l, x, r) = sf.split1(pred, accVm)
          (deepR(pr, m, l), x, r.map( _.toTree ).getOrElse( empty ))
        }
      }
    }
  )

  def isEmpty = fold(v => true, (v, x) => false, (v, pr, m, sf) => false)

//   private type ({type λ[α]=FingerTree[V, α]})#λ = ({type λ[α]=FingerTree[V, α]})#λ

  def viewl: ViewL[({type λ[α]=FingerTree[V, α]})#λ, A] =
    fold(
      v => EmptyL[({type λ[α]=FingerTree[V, α]})#λ, A],
      (v, x) => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](x, empty[V, A]),
      (v, pr, m, sf) =>
        pr match {
          case One(v, x) => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](x, rotL(m, sf))
          case _ => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](pr.lhead, deep(pr.ltail, m, sf))
        })

  def viewr: ViewR[({type λ[α]=FingerTree[V, α]})#λ, A] =
    fold(
      v => EmptyR[({type λ[α]=FingerTree[V, α]})#λ, A],
      (v, x) => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](empty[V, A], x),
      (v, pr, m, sf) =>
        sf match {
          case One(v, x) => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](rotR(pr, m), x)
          case _ => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](deep(pr, m, sf.rtail), sf.rhead)
        })

  def head = viewl.head

  def last = viewr.last

  def tail = viewl.tail

  def init = viewr.init

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): FingerTree[V2, B] = {
    implicit val nm = NodeMeasure[B, V2]
    fold(
      v => empty,
      (v, x) => single(f(x)),
      (v, pr, mt, sf) => deep(pr map f, mt.map(x => x.map(f)), sf map f))
  }

  def foreach(f: A => Unit) {
    fold(
      _ => {},
      (_, x) => { f(x) },
      (_, pr, m, sf) => { pr.foreach(f); m.foreach(_.foreach(f)); sf.foreach(f) }
    )}

  def iterator: Iterator[A] = fold(
    _ => Iterator.empty,
    (_, x) => Iterator.single(x),
    (_, pr, m, sf) => pr.iterator ++ m.iterator.flatMap(_.iterator) ++ sf.iterator)

  def reverseIterator: Iterator[A] = fold(
    _ => Iterator.empty,
    (_, x) => Iterator.single(x),
    (_, pr, m, sf) => sf.reverseIterator ++ m.reverseIterator.flatMap(_.reverseIterator) ++ pr.reverseIterator)

  import scala.collection.immutable.Stream
/* HH  import scala.collection.immutable.Stream._ */

  def toStream: Stream[A] = map(x => x)(StreamReducer[A]).measure
  def toList: List[A] = toStream.toList

  import FingerTree._

   /* HH
  override def toString = {
    implicit val v = showA[V]
    implicit val a = showA[A]
    this.shows
  }
  */
}

/* HH
class FingerTreeIntPlus[A](val value: FingerTree[Int, A]) {
  // A placeholder for a FingerTree specialized to the (Int, +) monoid
  // Will need to see how much it helps performance
}
*/

object FingerTree {
/* HH
  implicit def FingerTreeShow[V: Show, A: Show]: Show[FingerTree[V,A]] = shows((t: FingerTree[V,A]) => t.fold(
    empty = v => v + " []",
    single = (v, x) => v + " [" + x.shows + "]",
    deep = (v, pf, m, sf) => v + " [" + pf.toList.shows + ", ?, " + sf.toList.shows + "]"
  ))
*/
  def Node2[V, A](v: V, a1: => A, a2: => A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B) =
      two(v, a1, a2)
    val measure = v
  }

  def Node3[V, A](v: V, a1: => A, a2: => A, a3: => A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B) =
      three(v, a1, a2, a3)
    val measure = v
  }

  def EmptyR[S[_], A]: ViewR[S, A] = new ViewR[S, A] {
    def fold[B](b: => B, f: (=> S[A], => A) => B) = b
  }

  def OnR[S[_], A](sa: => S[A], a: => A): ViewR[S, A] = new ViewR[S, A] {
    def fold[B](b: => B, f: (=> S[A], => A) => B) = f(sa, a)
  }

  def EmptyL[S[_], A]: ViewL[S, A] = new ViewL[S, A] {
    def fold[B](b: => B, f: (=> A, => S[A]) => B) = b
  }

  def OnL[S[_], A](a: => A, sa: => S[A]): ViewL[S, A] = new ViewL[S, A] {
    def fold[B](b: => B, f: (=> A, => S[A]) => B) = f(a, sa)
  }

/* HH
  implicit def FingerFoldable[V] = new Foldable[({type λ[α]=FingerTree[V, α]})#λ] {
    override def foldMap[A, M: Monoid](v: Finger[V, A], f: A => M) = v.foldMap(f)
  }
*/
  implicit def FingerMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Finger[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: Finger[V, A]) => a.measure)
  }

  implicit def NodeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Node[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: Node[V, A]) => a fold (
            (v, _, _) => v,
            (v, _, _, _) => v))
  }

  implicit def FingerTreeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[FingerTree[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: FingerTree[V, A]) => a.fold(v => v, (v, x) => v, (v, x, y, z) => v))
  }

  def one[V, A](a: => A)(implicit measure: Reducer[A, V]) =
    One(a.unit[V], a)

  def two[V, A](a1: => A, a2: => A)(implicit measure: Reducer[A, V]) =
    Two(a1.unit[V] snoc a2, a1, a2)

  def three[V, A](a1: => A, a2: => A, a3: => A)(implicit measure: Reducer[A, V]) =
    Three(a1.unit[V] snoc a2 snoc a3, a1, a2, a3)

  def four[V, A](a1: => A, a2: => A, a3: => A, a4: => A)(implicit measure: Reducer[A, V]) =
    Four(a1.unit[V] snoc a2 snoc a3 snoc a4, a1, a2, a3, a4)

  def node2[V, A](a: => A, b: => A)(implicit measure: Reducer[A, V]) =
    Node2[V, A](a.unit[V] snoc b, a, b)

  def node3[V, A](a: => A, b: => A, c: => A)(implicit measure: Reducer[A, V]) =
    Node3[V, A](a.unit[V] snoc b snoc c, a, b, c)

  private def mappendVal[V, A](v: V, t: FingerTree[V, A])(implicit measure: Reducer[A, V]) = {
    t.fold(x => v, (x, y) => v snoc t, (x, p, m, s) => v snoc t)
  }

  def empty[V, A](implicit ms: Reducer[A, V]) = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = b(ms.monoid.zero)
  }

   def single[V, A](a: => A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = single(a.unit[V], a)

  def single[V, A](v: V, a: => A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = s(v, a)
  }

  def deep[V, A](pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] = {
    deep(mappendVal(pr.unit[V], m) snoc sf, pr, m, sf)
  }

  def deep[V, A](v: V, pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      implicit val nodeMeasure = NodeMeasure[A, V]
      lazy val mz = m
      def fold[B](b: V => B, f: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B =
        d(v, pr, mz, sf)
    }

  private def deepL[V, A](mpr: Option[Finger[V, A]], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    mpr match {
      case None => rotL(m, sf)
      case Some(pr) => deep(pr, m, sf)
    }

  private def deepR[V, A](pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], msf: Option[Finger[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    msf match {
      case None => rotR(pr, m)
      case Some(sf) => deep(pr, m, sf)
    }

  private def rotL[V, A](m: FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewl.fold(
      sf.toTree,
      (a, mm) => deep(m.measure snoc sf, a.toDigit, mm, sf))

  private def rotR[V, A](pr: Finger[V, A], m: FingerTree[V, Node[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewr.fold(
      pr.toTree,
      (mm, a) => deep(mappendVal(pr.measure, m), pr, mm, a.toDigit))

/* HH
  implicit def ft2ftip[A](ft: FingerTree[Int, A]): FingerTreeIntPlus[A] = new FingerTreeIntPlus(ft)

  implicit def ftip2ft[A](ft: FingerTreeIntPlus[A]): FingerTree[Int, A] = ft.value
*/

   /* HH : removed ropes */

   ///////////////////////////////////////////////////////
   // the following has been added by Hanns Holger Rutz //
   ///////////////////////////////////////////////////////

   // --------------------- Indexed ---------------------

   sealed trait Indexed[ @specialized A ] {
      import Indexed._

      private type FT = FingerTree[ Int, A ]

      val value: FT

       def apply( i: Int ) : A = value.split( _ > i )._2.viewl.headOption
          .getOrElse( throw new IndexOutOfBoundsException( i.toString ))

      def ++( xs: Indexed[ A ]) = indSeq( value <++> xs.value )
      def :+( x: => A ) = indSeq( value :+ x )
      def +:( x: => A ) = indSeq( x +: value )

/* HH
      def tail = indSeq(value.tail)
      def init = indSeq(value.init)
      def map[B](f: A => B) : Indexed[ A ] = indSeq(value map f)
      def flatMap[B](f: A => IndSeq[B]) =
        indSeq(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x) /* f(x).value */))
*/

      def size: Int = value.measure

      private def splitAt0( i: Int ) : (FT, FT) = value.split( _ > i )

      def splitAt( i: Int ) : (Indexed[ A ], Indexed[ A ]) = {
         val (l, r) = splitAt0( i )
         (indSeq( l ), indSeq( r ))
      }
   }

   object Indexed {
      def empty[ A ] : Indexed[ A ] = apply()

      def apply[ @specialized A ]( as: A* ) : Indexed[ A ] =
         indSeq( as.foldLeft( FingerTree.empty[ Int, A ]( Reducer( _ => 1 )))( (x, y) => x :+ y ))

      private def indSeq[ A ]( t: FingerTree[ Int, A ]) = new Indexed[ A ] {
         val value = t
      }
   }

   // --------------------- IndexedSummed ---------------------

   sealed trait IndexedSummed[ @specialized A, @specialized B ] {
      import IndexedSummed._

      private type FT = FingerTree[ (Int, B), A ]

      val value: FT

      def apply( i: Int ) : A = value.split( _._1 > i )._2.viewl.headOption
         .getOrElse( throw new IndexOutOfBoundsException( i.toString ))

      def ++( xs: IndexedSummed[ A, B ]) = indSeq( value <++> xs.value )
      def :+( x: => A ) = indSeq( value :+ x )
      def +:( x: => A ) = indSeq( x +: value )

      def size: Int = value.measure._1
      def sum: B = value.measure._2

      private def splitAt0( i: Int ) : (FT, FT) = value.split( _._1 > i )

      def splitAt( i: Int ) : (IndexedSummed[ A, B ], IndexedSummed[ A, B ]) = {
         val (l, r) = splitAt0( i )
         (indSeq( l ), indSeq( r ))
      }
   }

   object IndexedSummed {
      def empty[ A ]( implicit num: Numeric[ A ]): IndexedSummed[ A, A ] = apply()

      def apply[ @specialized A ]( as: A* )( implicit num: Numeric[ A ]) : IndexedSummed[ A, A ] = {
         implicit val m = monoid( num )
         apply( as, Reducer( (a: A) => (1, a) ))
      }

      def emptyWithView[ A, B ]( implicit num: Numeric[ B ], view: A => B ): IndexedSummed[ A, B ] = applyWithView()

      /**
       * Constructor which allows an indirection from sequence values to numeric views
       * which can be summed. This allows for instance to use numeric widening, as the
       * following example shows (sequence elements are @Int@s, but summing is performed
       * over @Long@s):
       * {{{
       * applyWithView( 2, 3, 5, 8 )( math.Numeric.LongIsIntegral, _.toLong )
       * }}}
       */
      def applyWithView[ @specialized A, @specialized B ]( as: A* )( implicit num: Numeric[ B ], view: A => B ) : IndexedSummed[ A, B ] = {
         implicit val m = monoid( num )
         apply( as, Reducer( (a: A) => (1, a: B) ))
      }

      private def monoid[ X ]( num: Numeric[ X ]) = new Monoid[ (Int, X) ] {
         def append( s1: (Int, X), s2: => (Int, X) ) = (s1._1 + s2._1, num.plus( s1._2, s2._2 ))
         val zero = (0, num.zero)
      }

      private def apply[ @specialized A, @specialized B ]( as: Seq[ A ], r: Reducer[ A, (Int, B) ]) : IndexedSummed[ A, B ] =
         indSeq( as.foldLeft( FingerTree.empty[ (Int, B), A ]( r ))( (x, y) => x :+ y ))

      private def indSeq[ A, B ]( t: FingerTree[ (Int, B), A ]) = new IndexedSummed[ A, B ] {
         val value = t
      }
   }

   // --------------------- Ordered ---------------------

   sealed trait Ordered[ @specialized A ] {
      import Ordered._

      private type FT = FingerTree[ Option[ A ], A ]

      val value: FT
      implicit val ord: Ordering[ A ]

      private def splitAt0( a: A ) : (FT, FT) =
         value.split( _.map( ord.gteq( _, a )).getOrElse( false ))

      def splitAt( a: A ) : (Ordered[ A ], Ordered[ A ]) = {
         val (l, r) = splitAt0( a )
         (ordSeq( l ), ordSeq( r ))
      }

      def +(a: A) : Ordered[A] = {
         val (l, r) = splitAt0( a )
         ordSeq( l <++> (a +: r) )
      }
//      def ++(xs: Ordered[A]) = xs.toList.foldLeft(this)(_ insert _)
   }

   object Ordered {
//      private def empty[A](implicit ms: Reducer[A, Option[A]]) = new Ordered[A] {
//        def fold[B](b: Option[A] => B, s: (Option[A], A) => B, d: (Option[A], Finger[Option[A], A], =>
//           FingerTree[Option[A], Node[Option[A], A]], Finger[Option[A], A]) => B): B = b(ms.monoid.zero)
//      }

      def empty[ A ]( implicit ordering: Ordering[ A ]): Ordered[ A ] = apply()

      def apply[ @specialized A ]( xs: A* )( implicit ordering: Ordering[ A ]): Ordered[ A ] = {
         implicit val keyMonoid = new Monoid[ Option[ A ]] {
            def append( k1: Option[ A ], k2: => Option[ A ]) = k2 orElse k1
            val zero: Option[ A ] = None // none
         }
         implicit val keyer = Reducer( (a: A) => { val res: Option[A] = Some(a); res })
         xs.foldLeft( ordSeq( FingerTree.empty[ Option[ A ], A ]))( _ + _ )
      }

      private def ordSeq[ A ]( t: FingerTree[ Option[ A ], A ])( implicit ordering: Ordering[ A ]) = new Ordered[ A ] {
         val value   = t
         val ord     = ordering
      }
   }

   // --------------------- Ranged ---------------------

   sealed trait Ranged[ @specialized A ] {
      import Ranged._

      private type I = (A, A)
      private type FT = FingerTree[ Anno[ A ], I ]
      val value: FT
      implicit val ord: Ordering[ A ]

      // "We order the intervals by their low endpoints"
      private def splitAt0( i: I ) : (FT, FT) = {
         val iLo = i._1
         value.split( _._1.map( ord.gteq( _, iLo )).getOrElse( false ))
      }

      def +( i: I ) : Ranged[ A ] = {
// XXX should have Interval wrapper that does this check
//         require( ord.lteq( i._1, i._2 ), "Upper interval bound cannot be less than lower bound : " + i )
         val (l, r) = splitAt0( i )
         rangedSeq( l <++> (i +: r) )
      }

      /* TODO:
            this should be renamed to findTouching
            and findOverlap should change the semantics
            from lteq to lt!
       */
      def findOverlap( i: I ) : Option[ I ] = {
         value.measure._2 flatMap { tHi =>
            val (iLo, iHi) = i
            // if the search interval's low bound is smaller or equal than the tree's total up bound...
            if( ord.lteq( iLo, tHi )) {
               // "gives us the interval x with the smallest low endpoint
               //  whose high endpoint is at least the low endpoint of the query interval"
               //
               // Note: n <= MInfty is always false. Since MInfty is equivalent to None
               //   in our implementation, we can write _.map( ... ).getOrElse( false )
               //   for this test
               val (_, x, _) = value.split1( atleast( iLo ) _, value.measure )
               // "It then remains to check that low x <= high i"
               if( ord.lteq( x._1, iHi )) Some( x ) else None
            } else None
         }
      }

//      /*
//         TODO:
//
//         should return a Stream probably? at least something lazy
//       */
//      def filterOverlap( i: I ) : List[ I ] = {
////         matches (takeUntil (greater (high i)) t)
////         where matches xs = case viewL (dropUntil (atleast (low i)) xs) of
////            Nil L	→ [ ]
////            ConsL x xs′ → x : matches xs′
//
//         val (iLo, iHi) = i
//
//         def matches( xs: FT ) : List[ I ] = {
//            val v = xs.dropUntil( atleast( iLo ) _ ).viewl
////            (v.headOption, v.tailOption) match {  // XXX efficient?
////               case (Some( x ), Some( xs0 )) => x :: matches( xs0 )  // XXX tailrec!
////               case _ => Nil
////            }
//            v.fold( Nil, (x, xs0) => x :: matches( xs0 ))  // XXX tailrec!
//         }
//         matches( value.takeUntil( greater( iHi ) _ ))
//      }

      def filterOverlap( i: I ) : Stream[ I ] = {
         val (iLo, iHi) = i

         def matches( xs: FT ) : Stream[ I ] = {
            val v = xs.dropUntil( atleast( iLo ) _ ).viewl
            v.fold( Stream.empty, (x, xs0) => Stream.cons( x, matches( xs0 )))
         }
         matches( value.takeUntil( greater( iHi ) _ ))
      }

      @inline private def atleast( k: A )( v: Anno[ A ]) = v._2.map( ord.lteq( k, _ )).getOrElse( false )
      @inline private def greater( k: A )( v: Anno[ A ]) = v._1.map( ord.gt( _, k )).getOrElse( false )
   }

   object Ranged {
      private type Anno[ A ] = (Option[ A ], Option[ A ])  

      def empty[ A ]( implicit ordering: Ordering[ A ]): Ranged[ A ] = apply()

      def apply[ @specialized A ]( xs: (A, A)* )( implicit ordering: Ordering[ A ]): Ranged[ A ] = {
         implicit val keyMonoid = new Monoid[ Option[ A ]] {
            def append(k1: Option[ A ], k2: => Option[ A ]) = k2 orElse k1
            val zero: Option[ A ] = None // none
         }
         implicit val keyer = Reducer( (a: (A, A)) => { val res: Anno[A] = (Some( a._1 ), Some( a._2 )); res })
         xs.foldLeft( rangedSeq( FingerTree.empty[ Anno[ A ], (A, A) ]))( _ + _ )
      }

      private def rangedSeq[ A ](t: FingerTree[ Anno[ A ], (A, A) ])( implicit ordering: Ordering[ A ]) = new Ranged[ A ] {
         val value   = t
         val ord     = ordering
      }
   }
}