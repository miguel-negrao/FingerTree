/*
 * FingerTree.scala
 * (FingerTree)
 *
 * Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either
 * version 2, june 1991 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License (gpl.txt) along with this software; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.fingertree

/**
 * Variant of a finger tree which adds a measure.
 */
object FingerTree {
   def empty[ V, A ]( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = new Empty[ V ]( m.zero )

   implicit private def nodeMeasure[ V, A ]( implicit m: Measure[ A, V ]) : Measure[ Digit[ V, A ], V ] =
      new Measure[ Digit[ V, A ], V ] {
         def zero : V = m.zero
         def |+|( a: V, b: V ) : V = m.|+|( a, b )
         def unit( n: Digit[ V, A ]) : V = n.measure
      }

   // ---- Trees ----

   final private case class Single[ V, A ]( measure: V, a: A ) extends FingerTree[ V, A ] {
      def head = a
      def headOption : Option[ A ] = Some( a )
      def tail(  implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = empty[ V, A ]

      def last = a
      def lastOption : Option[ A ] = Some( a )
      def init( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = empty[ V, A ]

      def isEmpty = false

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vPrefix = m.unit( b )
         val prefix  = One( vPrefix, b )
         val vSuffix = m.unit( a )
         val suffix = One( vSuffix, a )
         Deep( m.|+|( vPrefix, vSuffix ), prefix, empty[ V, Digit[ V, A1 ]], suffix )
      }

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vPrefix = m.unit( a )
         val prefix  = One( vPrefix, a )
         val vSuffix = m.unit( b )
         val suffix  = One( vSuffix, b )
         Deep( m.|+|( vPrefix, vSuffix ), prefix, empty[ V, Digit[ V, A1 ]], suffix )
      }

      def viewLeft(  implicit m: Measure[ A, V ]) : ViewLeft[  V, A ] = ViewConsLeft[  V, A ]( a, empty[ V, A ])
      def viewRight( implicit m: Measure[ A, V ]) : ViewRight[ V, A ] = ViewConsRight[ V, A ]( empty[ V, A ], a )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a
         }
      }

      def toList : List[ A ] = a :: Nil

      override def toString = "(" + a + ")"
   }

   final private case class Deep[ V, A ]( measure: V, prefix: Digit[ V, A ], tree: FingerTree[ V, Digit[ V, A ]],
                                          suffix: Digit[ V, A ])
   extends FingerTree[ V, A ] {

      def isEmpty    = false

      def head       = prefix.head
      def headOption : Option[ A ] = Some( prefix.head )
      def last       = suffix.last
      def lastOption : Option[ A ] = Some( suffix.last )

      def tail( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = viewLeft.tail
      def init( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = viewRight.tail

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vb   = m.unit( b )
         val vNew = m.|+|( vb, measure )
         prefix match {
            case Four( _, d, e, f, g ) =>
               val prefix     = Two( m.|+|( vb, m.unit( d )), b, d )
               val vTreePefix = m.|+|( m.|+|( m.unit( e ), m.unit( f )), m.unit( g ))
               val treeNew    = tree.+:[ Digit[ V, A1 ]]( Three( vTreePefix, e, f, g ))
               Deep( vNew, prefix, treeNew, suffix )

            case partial =>
               Deep( vNew, b +: partial, tree, suffix )
         }
      }

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = {
         val vb   = m.unit( b )
         val vNew = m.|+|( vb, measure )
         suffix match {
            case Four( _, g, f, e, d ) =>
               val vTreeSuffix= m.|+|( m.|+|( m.unit( g ), m.unit( f )), m.unit( e ))
               val treeNew    = tree.:+[ Digit[ V, A1 ]]( Three( vTreeSuffix, g, f, e ))
               val suffix     = Two( m.|+|( m.unit( d ), vb ), d, b )
               Deep( vNew, prefix, treeNew, suffix )
            case partial =>
               Deep( vNew, prefix, tree, partial :+ b )
         }
      }

      def viewLeft( implicit m: Measure[ A, V ]) : ViewLeft[ V, A ] = {
         def deep( prefix: Digit[ V, A ], tree: FingerTree[ V, Digit[ V, A ]], suffix: Digit[ V, A ]) = prefix match {
            case One( _, _ ) => tree.viewLeft match {
               case ViewConsLeft( a, newTree ) =>
                  val vNew = m.|+|( m.|+|( a.measure, newTree.measure ), suffix.measure )
                  Deep( vNew, a, newTree, suffix )
               case _ =>
                  suffix.toTree
            }

            case _prefix =>
               val prefixNew = _prefix.tail
               val vNew = m.|+|( m.|+|( prefixNew.measure, tree.measure ), suffix.measure )
               Deep( vNew, prefixNew, tree, suffix )
         }

         ViewConsLeft( prefix.head, deep( prefix, tree, suffix ))
      }

      def viewRight( implicit m: Measure[ A, V ]) : ViewRight[ V, A ] = {
         def deep( prefix: Digit[ V, A ], tree: FingerTree[ V, Digit[ V, A ]], suffix: Digit[ V, A ]) = suffix match {
            case One( _, _ ) => tree.viewRight match {
               case ViewConsRight( newTree, a ) =>
                  val vNew = m.|+|( m.|+|( prefix.measure, newTree.measure ), a.measure )
                  Deep( vNew, prefix, newTree, a )
               case _ =>
                  prefix.toTree
            }

            case _suffix =>
               val suffixNew = _suffix.init
               val vNew = m.|+|( m.|+|( prefix.measure, tree.measure ), suffixNew.measure )
               Deep( vNew, prefix, tree, suffixNew )
         }

         ViewConsRight( deep( prefix, tree, suffix.init ), suffix.last )
      }

      def iterator : Iterator[ A ] = prefix.iterator ++ (tree.iterator flatMap { _.toList.iterator }) ++ suffix.iterator

      def toList : List[ A ] = iterator.toList

      override def toString = "(" + prefix + ", " + tree + ", " + suffix + ")"
   }

   final private case class Empty[ V ]( measure: V ) extends FingerTree[ V, Nothing ] {
      def isEmpty = true

      def head = throw new NoSuchElementException( "head on empty finger tree" )
      def headOption : Option[ Nothing ] = None
      def tail( implicit m: Measure[ Nothing, V ]) : FingerTree[ V, Nothing ] =
         throw new NoSuchElementException( "tailLeft on empty finger tree" )

      def last = throw new NoSuchElementException( "last on empty finger tree" )
      def lastOption : Option[ Nothing ] = None
      def init( implicit m: Measure[ Nothing, V ]) : FingerTree[ V, Nothing ] =
         throw new NoSuchElementException( "tailRight on empty finger tree" )

      def +:[ A1 ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = Single( m.unit( a1 ), a1 )
      def :+[ A1 ]( a1: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ] = Single( m.unit( a1 ), a1 )

      def viewLeft(  implicit m: Measure[ Nothing, V ]) : ViewLeft[  V, Nothing ] = ViewNilLeft[  V ]()
      def viewRight( implicit m: Measure[ Nothing, V ]) : ViewRight[ V, Nothing ] = ViewNilRight[ V ]()

      def iterator : Iterator[ Nothing ] = new Iterator[ Nothing ] {
         val hasNext = false

         def next = throw new NoSuchElementException
      }

      def toList : List[ Nothing ] = Nil

      override def toString = "()"
   }

   // ---- Views ----

   sealed trait ViewLeft[ V, +A ] {
      def head : A
      def tail : FingerTree[ V, A ]
   }

   final case class ViewConsLeft[ V, A ]( head: A, tail: FingerTree[ V, A ]) extends ViewLeft[ V, A ]

   final case class ViewNilLeft[ V ]() extends ViewLeft[ V, Nothing ] {
      def head : Nothing                  = throw new NoSuchElementException( "head on empty view" )
      def tail : FingerTree[ V, Nothing ] = throw new NoSuchElementException( "tail on empty view" )
   }

   sealed trait ViewRight[ V, +A ] {
      def tail : FingerTree[ V, A ]
      def head : A
   }

   final case class ViewConsRight[ V, A ]( tail: FingerTree[ V, A ], head: A ) extends ViewRight[ V, A ]

   final case class ViewNilRight[ V ]() extends ViewRight[ V, Nothing ] {
      def tail : FingerTree[ V, Nothing ] = throw new NoSuchElementException( "tail on empty view" )
      def head : Nothing                  = throw new NoSuchElementException( "head on empty view" )
   }

   // ---- Digits ----

   sealed trait Digit[ V, +A ] {
      def measure: V

      def head : A
      def tail( implicit m: Measure[ A, V ]) : Digit[ V, A ]

      def last : A
      def init( implicit m: Measure[ A, V ]) : Digit[ V, A ]

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ]
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ]

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ]

      def toList : List[ A ]

      def iterator : Iterator[ A ]
   }

   final case class One[ V, A ]( measure: V, a1: A ) extends Digit[ V, A ] {
      def head  = a1
      def tail( implicit m: Measure[ A, V ]) : Digit[ V, A ] = throw new NoSuchElementException( "tail on digit: one" )

      def last = a1
      def init( implicit m: Measure[ A, V ]) : Digit[ V, A ] = throw new NoSuchElementException( "tail on digit: one" )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Two( m.|+|( m.unit( b ), measure ), b, a1 )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Two( m.|+|( measure, m.unit( b )),  a1, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = Single( measure, a1 )

      def toList : List[ A ] = a1 :: Nil

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var hasNext = true

         def next = {
            hasNext = false
            a1
         }
      }

      override def toString = "(" + a1 + ")"
   }

   final case class Two[ V, A ]( measure: V, a1: A, a2: A ) extends Digit[ V, A ] {
      def head  = a1
      def tail( implicit m: Measure[ A, V ]) : Digit[ V, A ] = One( m.unit( a2 ), a2 )

      def last = a2
      def init( implicit m: Measure[ A, V ]) : Digit[ V, A ] = One( m.unit( a1 ), a1 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Three( m.|+|( m.unit( b ), measure ), b, a1, a2 )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] = Three( m.|+|( measure, m.unit( b )),  a1, a2, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: Single( m.unit( a2 ), a2 )

      def toList : List[ A ] = a1 :: a2 :: Nil

      def iterator : Iterator[ A ] = toList.iterator

      override def toString = "(" + a1 + ", " + a2 + ")"
   }

   final case class Three[ V, A ]( measure: V, a1: A, a2: A, a3: A ) extends Digit[ V, A ] {
      def head  = a1
      def tail( implicit m: Measure[ A, V ]) : Digit[ V, A ] = Two( m.|+|( m.unit( a2 ), m.unit( a3 )), a2, a3 )

      def last = a3
      def init( implicit m: Measure[ A, V ]) : Digit[ V, A ] = Two( m.|+|( m.unit( a1 ), m.unit( a2 )), a1, a2 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] =
         Four( m.|+|( m.unit( b ), measure ), b, a1, a2, a3 )

      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : Digit[ V, A1 ] =
         Four( m.|+|( measure, m.unit( b )), a1, a2, a3, b )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: a2 +: Single( m.unit( a3 ), a3 )

      def toList : List[ A ] = a1 :: a2 :: a3 :: Nil

      def iterator : Iterator[ A ] = toList.iterator

      override def toString = "(" + a1 + ", " + a2 + ", " + a3 + ")"
   }

   final case class Four[ V, A ]( measure: V, a1: A, a2: A, a3: A, a4: A ) extends Digit[ V, A ] {
      def head  = a1
      def tail( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Three( m.|+|( m.|+|( m.unit( a2 ), m.unit( a3 )), m.unit( a4 )), a2, a3, a4 )

      def last = a4
      def init( implicit m: Measure[ A, V ]) : Digit[ V, A ] =
         Three( m.|+|( m.|+|( m.unit( a1 ), m.unit( a2 )), m.unit( a3 )), a1, a2, a3 )

      def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) = throw new UnsupportedOperationException( "+: on Four" )
      def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) = throw new UnsupportedOperationException( ":+ on Four" )

      def toTree( implicit m: Measure[ A, V ]) : FingerTree[ V, A ] = a1 +: a2 +: a3 +: Single( m.unit( a4 ), a4 )

      def toList : List[ A ] = a1 :: a2 :: a3 :: a4 :: Nil

      def iterator : Iterator[ A ] = toList.iterator

      override def toString = "(" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ")"
   }
}

sealed trait FingerTree[ V, +A ] {
   import FingerTree._

   /**
    * Queries whether the tree is empty or not
    *
    * @return  `true` if the tree is empty
    */
   def isEmpty : Boolean

   /**
    * Queries the measure of the tree, which might be its size or sum
    *
    * @return  the measure of the tree
    */
   def measure : V

   /**
    * Returns the first (left-most) element in the tree. Throws a runtime exception if performed on an empty tree.
    *
    * @return  the head element
    */
   def head : A

   /**
    * Returns the first (left-most) element in the tree as an option.
    *
    * @return  the head element (`Some`), or `None` if the tree is empty
    */
   def headOption : Option[ A ]

   /**
    * Returns a copy of the tree with the first (head) element removed. Throws a runtime exception if performed
    * on an empty tree.
    *
    * @param m the measure used to update the tree's structure
    * @return  the new tree with the first element removed
    */
   def tail( implicit m: Measure[ A, V ]) : FingerTree[ V, A ]

   /**
    * Returns the last (right-most) element in the tree. Throws a runtime exception if performed on an empty tree.
    *
    * @return  the last element
    */
   def last : A

   /**
    * Returns the last (right-most) element in the tree as an option.
    *
    * @return  the last element (`Some`), or `None` if the tree is empty
    */
   def lastOption : Option[ A ]
   def init( implicit m: Measure[ A, V ]) : FingerTree[ V, A ]

   /**
    * Prepends an element to the tree.
    *
    * @param b the element to prepend
    * @param m the measure used to update the tree's measure
    * @return  the new tree with the element prepended
    */
   def +:[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ]

   /**
    * Appends an element to the tree.
    *
    * @param b the element to append
    * @param m the measure used to update the tree's structure
    * @return  the new tree with the element appended
    */
   def :+[ A1 >: A ]( b: A1 )( implicit m: Measure[ A1, V ]) : FingerTree[ V, A1 ]

   def viewLeft(  implicit m: Measure[ A, V ]): ViewLeft[  V, A ]
   def viewRight( implicit m: Measure[ A, V ]): ViewRight[ V, A ]

   /**
    * Creates an `Iterator` over the elements of the tree
    *
    * @return  a fresh `Iterator` for the tree elements
    */
   def iterator: Iterator[ A ]

   /**
    * Converts the tree to a `List` representation.
    *
    * @return  a `List` constructed from the elements in the tree
    */
   def toList : List[ A ]
}