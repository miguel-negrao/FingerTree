package de.sciss.fingertree

object IndexedSummedSeq {
   val emptyIntLong : IndexedSummedSeq[ Int, Long ] = {
      implicit val m = Measure.IndexedSummedIntLong
      new Impl[ Int, Long ]( FingerTree.empty )
   }

   def empty[ Elem, Sum ]( implicit m: Measure[ Elem, Sum ]) : IndexedSummedSeq[ Elem, Sum ] = {
      implicit val m2 = Measure.Indexed.zip( m )
      new Impl[ Elem, Sum ]( FingerTree.empty[ (Int, Sum), Elem ])
   }

   private final class Impl[ Elem, Sum ]( protected val tree: FingerTree[ (Int, Sum), Elem ])
                                        ( implicit protected val m: Measure[ Elem, (Int, Sum) ])
   extends IndexedSummedSeq[ Elem, Sum ] {
      protected def wrap( tree: FingerTree[ (Int, Sum), Elem ]) : IndexedSummedSeq[ Elem, Sum ] = new Impl( tree )
      protected def splitTreeAt( i: Int ) = tree.split(  _._1 > i )
      protected def splitTree1(  i: Int ) = tree.split1( _._1 > i )
      protected def apply1( i: Int )      = tree.find1(  _._1 > i )

      def size : Int = tree.measure._1
      def sum : Sum  = tree.measure._2

      override def toString = tree.iterator.mkString( "Seq<sum=" + sum + ">(", ", ", ")" )
   }
}
sealed trait IndexedSummedSeq[ Elem, Sum ] extends IndexedSeqLike[ (Int, Sum), Elem, IndexedSummedSeq[ Elem, Sum ]] {
   def sum: Sum
}