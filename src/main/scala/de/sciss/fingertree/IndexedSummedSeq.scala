package de.sciss.fingertree

object IndexedSummedSeq {
   val emptyIntLong : IndexedSummedSeq[ Int, Long ] = {
      implicit val m = Measure.IndexedSummedIntLong
      new Impl[ Int, Long ]( FingerTree.empty )
   }

   def applyIntLong( elems: Int* ) : IndexedSummedSeq[ Int, Long ] = {
      implicit val m = Measure.IndexedSummedIntLong
      new Impl[ Int, Long ]( FingerTree( elems: _* ))
   }

   def empty[ Elem, Sum ]( implicit m: Measure[ Elem, Sum ]) : IndexedSummedSeq[ Elem, Sum ] = {
      implicit val m2 = Measure.Indexed.zip( m )
      new Impl[ Elem, Sum ]( FingerTree.empty )
   }

   def apply[ Elem, Sum ]( elems: Elem* )( implicit m: Measure[ Elem, Sum ]) : IndexedSummedSeq[ Elem, Sum ] = {
      implicit val m2 = Measure.Indexed.zip( m )
      new Impl[ Elem, Sum ]( FingerTree( elems: _* ))
   }

   private final class Impl[ Elem, Sum ]( protected val tree: FingerTree[ (Int, Sum), Elem ])
                                        ( implicit protected val m: Measure[ Elem, (Int, Sum) ])
   extends IndexedSummedSeq[ Elem, Sum ] {
      protected def wrap( tree: FingerTree[ (Int, Sum), Elem ]) : IndexedSummedSeq[ Elem, Sum ] = new Impl( tree )
      protected def indexPred( i: Int ) = _._1 > i

      def size : Int = tree.measure._1
      def sum : Sum  = tree.measure._2

      override def toString = tree.iterator.mkString( "Seq<sum=" + sum + ">(", ", ", ")" )
   }
}
sealed trait IndexedSummedSeq[ Elem, Sum ] extends IndexedSeqLike[ (Int, Sum), Elem, IndexedSummedSeq[ Elem, Sum ]] {
   def sum: Sum
}