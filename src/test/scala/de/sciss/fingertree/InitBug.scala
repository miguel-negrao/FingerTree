package de.sciss.fingertree

object InitBug extends App {
   implicit val m = Measure.Unit
   val seq1    = FingerTree[ Unit, String ]( "a", "b", "c" )
   val seq1i   = seq1.init
   val seq2    = seq1.tail
   val seq2i   = seq2.init
   val seq3    = seq2.tail
   val seq3i   = seq3.init

   val seq1t   = seq1.tail

   println( "Aqui." )
}
