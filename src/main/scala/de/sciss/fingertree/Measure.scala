/*
 * Measure.scala
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

object Measure {
   object Unit extends Measure[ Any, Unit ] {
      override def toString = "Unit"
      val zero = ()
      def apply( c: Any ) {}
      def |+|( a: Unit, b: Unit ) {}
   }

   object Indexed extends Measure[ Any, Int ] {
      override def toString = "Indexed"
      val zero = 0
      def apply( c: Any ) = 1
      def |+|( a: Int, b: Int ) = a + b
   }

   object SummedIntLong extends Measure[ Int, Long ] {
      override def toString = "SummedIntLong"
      val zero = 0L
      def apply( c: Int ) = c.toLong
      def |+|( a: Long, b: Long ) = a + b
   }

   object IndexedSummedIntLong extends Measure[ Int, (Int, Long) ] {
      override def toString = "IndexedSummedIntLong"
      val zero = (0, 0L)
      def apply( c: Int ) = (1, c.toLong)
      def |+|( a: (Int, Long), b: (Int, Long) ) = ((a._1 + b._1), (a._2 + b._2))
   }

   private final class Zip[ C, M, N ]( m1: Measure[ C, M ], m2: Measure[ C, N ])
   extends Measure[ C, (M, N) ] {
      override def toString = "(" + m1 + " zip " + m2 + ")"
      def zero = (m1.zero, m2.zero)
      def apply( c: C ) = (m1.apply( c ), m2.apply( c ))
      def |+|( a: (M, N), b: (M, N) ) = (m1.|+|( a._1, b._1 ), m2.|+|( a._2, b._2 ))
   }
}
trait Measure[ -C, M ] {
   import Measure._

   def zero: M
   def apply( c: C ) : M
   def |+|( a: M, b: M ) : M

   final def zip[ C1 <: C, N ]( m: Measure[ C1, N ]) : Measure[ C1, (M, N) ] = new Zip( this, m )
}
