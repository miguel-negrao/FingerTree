/*
   ///////////////////////////////////////////////////////
   // the following has been added by Hanns Holger Rutz //
   ///////////////////////////////////////////////////////

   // --------------------- Indexed ---------------------

   sealed trait Wrapped[ V, A, Repr <: Wrapped[ V, A, Repr ]] {
      def iterator: Iterator[ A ] = tree.iterator
      def isEmpty: Boolean = tree.isEmpty
      def nonEmpty: Boolean = !isEmpty

      def head: A = tree.head
      def headOption: Option[ A ] = tree.headOption

      def last: A = tree.last
      def lastOption: Option[ A ] = tree.lastOption

      def init: Repr = wrap( tree.init )
      def tail: Repr = wrap( tree.tail )

      def foreach[ U ]( f: A => U ) { tree.foreach( f )}

      def toList : List[ A ] = tree.toList
      def toStream : Stream[ A ] = tree.toStream

      protected def tree: FingerTree[ V, A ]
      protected def wrap( tree: FingerTree[ V, A ]) : Repr
   }

   sealed trait IndexedLike[ V, A, Repr <: IndexedLike[ V, A, Repr ]] extends Wrapped[ V, A, Repr ] {
      def ++( xs: Repr ): Repr = wrap( tree <++> xs.tree )
//      def :+( x: => A ): Repr = wrap( tree :+ x )
//      def +:( x: => A ): Repr = wrap( x +: tree )
      def :+( x: A ): Repr = wrap( tree :+ x )
      def +:( x: A ): Repr = wrap( x +: tree )

      override def isEmpty: Boolean = size == 0

      def apply( idx: Int ) : A = {
         if( idx < 0 || idx >= size ) throw new IndexOutOfBoundsException( idx.toString )
         val (_, x, _) = splitTree1( idx )
         x
      }

      def size : Int

      def drop( n: Int ) : Repr = wrap( splitTreeAt( n )._2 )
      def dropRight( n: Int ) : Repr = wrap( splitTreeAt( size - n )._1 )
      def slice( from: Int, until: Int ) : Repr = dropRight( size - until ).drop( from ) // XXX most efficient?

      def splitAt( i: Int ) : (Repr, Repr) = {
         val (l, r) = splitTreeAt( i )
         (wrap( l ), wrap( r ))
      }

      def take( n: Int ) : Repr = wrap( splitTreeAt( n )._1 )
      def takeRight( n: Int ) : Repr = wrap( splitTreeAt( size - n )._2 )

      def updated( index: Int, elem: A ) : Repr = {
         if( index < 0 || index >= size ) throw new IndexOutOfBoundsException( index.toString )
         val (l, _, r) = splitTree1( index )
         wrap( l.:+( elem ).<++>( r ))  // XXX most efficient?
      }

      protected def splitTreeAt( i: Int ) : (FingerTree[ V, A ], FingerTree[ V, A ])
      protected def splitTree1( i: Int ) : (FingerTree[ V, A ], A, FingerTree[ V, A ])
   }

   sealed trait Indexed[ @specialized A ] extends IndexedLike[ Int, A, Indexed[ A ]] {
      import Indexed._

/* HH
      def map[B](f: A => B) : Indexed[ A ] = indSeq(value map f)
      def flatMap[B](f: A => IndSeq[B]) =
        indSeq(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x) /* f(x).value */))
*/

      def size: Int = tree.measure

      protected def splitTreeAt( i: Int ) = tree.split( _ > i )
      protected def splitTree1( i: Int ) = tree.split1( _ > i )
      protected def wrap( tree: FingerTree[ Int, A ] ) = indSeq( tree )
   }

   object Indexed {
      def empty[ A ] : Indexed[ A ] = apply()

      def apply[ @specialized A ]( as: A* ) : Indexed[ A ] =
         indSeq( as.foldLeft( FingerTree.empty[ Int, A ]( Reducer( _ => 1 )))( (x, y) => x :+ y ))

      private def indSeq[ A ]( t: FingerTree[ Int, A ]) = new Indexed[ A ] {
         def tree = t

         override def toString = t.toString( "FingerTree.Indexed", this )
      }
   }

   // --------------------- IndexedSummed ---------------------

   sealed trait IndexedSummed[ @specialized A, @specialized B ] extends IndexedLike[ (Int, B), A, IndexedSummed[ A, B ]] {
      import IndexedSummed._

      def size: Int = tree.measure._1
      def sum: B = tree.measure._2

      protected def wrap( tree: FingerTree[ (Int, B), A ]) = indSeq( tree )
      protected def splitTreeAt( i: Int ) = tree.split( _._1 > i )
      protected def splitTree1( i: Int ) = tree.split1( _._1 > i )
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
         def tree = t

         override def toString = t.toString( "FingerTree.IndexedSummed", this )
      }
   }

   // --------------------- Ordered ---------------------

   sealed trait Ordered[ @specialized A ] extends Wrapped[ Option[ A ], A, Ordered[ A ]] {
      import Ordered._

      implicit def ord: Ordering[ A ]

      private def splitTreeAt( a: A ) =
         tree.split( _.map( ord.gteq( _, a )).getOrElse( false ))

      def splitAt( a: A ) : (Ordered[ A ], Ordered[ A ]) = {
         val (l, r) = splitTreeAt( a )
         (wrap( l ), wrap( r ))
      }

      def +(a: A) : Ordered[A] = {
         val (l, r) = splitTreeAt( a )
         wrap( l <++> (a +: r) )
      }
//      def ++(xs: Ordered[A]) = xs.toList.foldLeft(this)(_ insert _)

      protected def wrap( tree: FingerTree[ Option[ A ], A ]) = ordSeq( tree )
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
         def tree   = t
         def ord    = ordering

         override def toString = t.toString( "FingerTree.Ordered", this )
      }
   }

   // --------------------- Ranged ---------------------

   sealed trait Ranged[ @specialized A ] extends Wrapped[ (Option[ A ], Option[ A ]), (A, A), Ranged[ A ]] {
      import Ranged._

      private type I = (A, A)
      private type FT = FingerTree[ Anno[ A ], I ]
      implicit def ord: Ordering[ A ]

      // "We order the intervals by their low endpoints"
      private def splitTreeAt( i: I ) = {
         val iLo = i._1
         tree.split( _._1.map( ord.gteq( _, iLo )).getOrElse( false ))
      }

      def +( i: I ) : Ranged[ A ] = {
// XXX should have Interval wrapper that does this check
//         require( ord.lteq( i._1, i._2 ), "Upper interval bound cannot be less than lower bound : " + i )
         val (l, r) = splitTreeAt( i )
         wrap( l <++> (i +: r) )
      }

      /* TODO:
            this should be renamed to findTouching
            and findOverlap should change the semantics
            from lteq to lt!
       */
      def findOverlap( i: I ) : Option[ I ] = {
         tree.measure._2 flatMap { tHi =>
            val (iLo, iHi) = i
            // if the search interval's low bound is smaller or equal than the tree's total up bound...
            if( ord.lteq( iLo, tHi )) {
               // "gives us the interval x with the smallest low endpoint
               //  whose high endpoint is at least the low endpoint of the query interval"
               //
               // Note: n <= MInfty is always false. Since MInfty is equivalent to None
               //   in our implementation, we can write _.map( ... ).getOrElse( false )
               //   for this test
               val (_, x, _) = tree.split1( atleast( iLo ) _, tree.measure )
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
         matches( tree.takeUntil( greater( iHi ) _ ))
      }

      protected def wrap( tree: FT ) = rangedSeq( tree )

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
         def tree = t
         def ord  = ordering

         override def toString = t.toString( "FingerTree.Ranged", this )
      }
   }
 */
