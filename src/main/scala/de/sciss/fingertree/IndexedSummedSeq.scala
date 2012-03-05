package de.sciss.fingertree

object IndexedSummedSeq {
   def empty[ A ] : IndexedSummedSeq[ A ] = new Impl[ A ]

   private final class Impl[ A ] extends IndexedSummedSeq[ A ] {
      def apply( idx: Int ) : A = sys.error( "TODO" )
   }
}
sealed trait IndexedSummedSeq[ A ] {
   def apply( idx: Int ) : A
}