package de.sciss.fingertree

object TestScalaz extends App {
   val N    = 50000 // 100000
   val SEED = 0L
   val WARM = 2

   def t()  = System.currentTimeMillis()

   def testFill() : Long = {
      implicit val red = new Reducer[ Int, Unit ] {}
      var q    = FingerTree.empty[ Unit, Int ]
      val rnd  = new util.Random( SEED )
      val t1   = t()
      var i = 0; while( i < N ) {
         q +:= rnd.nextInt( 1000 )
      i += 1 }
      val t2   = t()
      assert( q.toList.size == N )
      t2 - t1
   }

   var r = 0; while( r < WARM ) {
      testFill()
   r += 1 }

   val perfTestFill = testFill()
   println( "Test fill with N = " + N + " (warm up = " + WARM + ") took " + perfTestFill + " ms." )
}