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
   object Indexed extends Measure[ Any, Int ] {
      val zero = 0
      def unit( c: Any ) = 1
      def |+|( a: Int, b: Int ) = a + b
   }
}
trait Measure[ -C, M ] {
   def zero: M
   def unit( c: C ) : M
//   def +:( c: C, m: M ) : M
//   def :+( m: M, c: C ) : M
   def |+|( a: M, b: M ) : M
}
