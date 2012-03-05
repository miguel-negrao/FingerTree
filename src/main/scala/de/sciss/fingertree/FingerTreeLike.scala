/*
 * FingerTreeLike.scala
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

trait FingerTreeLike[ V, A, Repr <: FingerTreeLike[ V, A, Repr ]] {
   protected implicit def m: Measure[ A, V ]

   final def iterator: Iterator[ A ] = tree.iterator
   final def isEmpty: Boolean = tree.isEmpty
   final def nonEmpty: Boolean = !isEmpty

   final def head: A = tree.head
   final def headOption: Option[ A ] = tree.headOption

   final def last: A = tree.last
   final def lastOption: Option[ A ] = tree.lastOption

   final def init: Repr = wrap( tree.init )
   final def tail: Repr = wrap( tree.tail )

//   final def foreach[ U ]( f: A => U ) { tree.foreach( f )}

   final def toList : List[ A ] = tree.toList
//   def toStream : Stream[ A ] = tree.toStream

   protected def tree: FingerTree[ V, A ]
   protected def wrap( tree: FingerTree[ V, A ]) : Repr
}
