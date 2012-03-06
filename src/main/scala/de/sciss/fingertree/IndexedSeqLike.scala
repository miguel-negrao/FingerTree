package de.sciss.fingertree

trait IndexedSeqLike[ V, A, Repr <: IndexedSeqLike[ V, A, Repr ]] extends FingerTreeLike[ V, A, Repr ] {
//   final def ++( xs: Repr ): Repr = wrap( tree <++> xs.tree )
   final def :+( x: A ): Repr = wrap( tree :+ x )
   final def +:( x: A ): Repr = wrap( x +: tree )

   final def apply( idx: Int ) : A = {
      if( idx < 0 || idx >= size ) throw new IndexOutOfBoundsException( idx.toString )
//      val (_, x, _) = splitTree1( idx )
//      x
      apply1( idx )
   }

   def size : Int

   final def drop( n: Int ) : Repr = wrap( splitTreeAt( n )._2 )
   final def dropRight( n: Int ) : Repr = wrap( splitTreeAt( size - n )._1 )
   final def slice( from: Int, until: Int ) : Repr = dropRight( size - until ).drop( from ) // XXX most efficient?

   final def splitAt( i: Int ) : (Repr, Repr) = {
      val (l, r) = splitTreeAt( i )
      (wrap( l ), wrap( r ))
   }

   final def take( n: Int ) : Repr = wrap( splitTreeAt( n )._1 )
   final def takeRight( n: Int ) : Repr = wrap( splitTreeAt( size - n )._2 )

//   final def updated( index: Int, elem: A ) : Repr = {
//      if( index < 0 || index >= size ) throw new IndexOutOfBoundsException( index.toString )
//      val (l, _, r) = splitTree1( index )
//      wrap( l.:+( elem ).<++>( r ))  // XXX most efficient?
//   }

   protected def splitTreeAt( i: Int ) : (FingerTree[ V, A ], FingerTree[ V, A ])
   protected def splitTree1( i: Int ) : (FingerTree[ V, A ], A, FingerTree[ V, A ])
   protected def apply1( i: Int ) : A
}