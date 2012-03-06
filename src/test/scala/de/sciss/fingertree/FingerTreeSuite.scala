package de.sciss.fingertree

import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * To run only this test:
 *
 * test-only de.sciss.fingertree.FingerTreeSuite
 */
class FingerTreeSuite extends FeatureSpec with GivenWhenThen {
   val SEED = 0L
   val N1   = 100000
   val N2   = 1000

   val rnd  = new util.Random( SEED )

   def createStructures : (IndexedSummedSeq[ Int, Long ], IIdxSeq[ Int ]) = {
      var finger  = IndexedSummedSeq.emptyIntLong
      var scala   = IIdxSeq.empty[ Int ]
      for( i <- 0 until N1 ) {
         val elem = rnd.nextInt()
         if( rnd.nextBoolean() ) {
            finger :+= elem
            scala  :+= elem
         } else {
            finger +:= elem
            scala  +:= elem
         }
      }
      (finger, scala)
   }

   feature( "Consistency of the finger tree structure is verified" ) {
      scenarioWithTime( "Build+Compare", "Comparing structure to plain brute-force Scala structure" ) {
         val (finger, scala) = createStructures

         when( "the elements are compared" )
         val iter = finger.iterator.zip( scala.iterator )
         val diff = iter.find { case (a, b) => a != b }
         then( "they should be all equal" )
         assert( diff.isEmpty, "Found different pair " + diff )

         when( "the sizes are queried" )
         val szf = finger.size
         val szs = scala.size
         then( "they should be the same, and equal to the specification N" )
         assert( szf == szs, "FingerTree had size " + szf + ", Scala Vector had size " + szs )
         assert( szf == N1, "Expected size was " + N1 + ", but actual size was " + szf )

         when( "the sums are queried" )
         val sumf = finger.sum
         val sums = scala.map( _.toLong ).sum
         then( "they should be the same" )
         assert( sumf == sums, "FingerTree says sum is " + sumf + ", but Scala Vector sum is " + sums )
      }
   }

   scenarioWithTime( "Slice", "Creating arbitrary slices of the tree" ) {
      val (finger, scala) = createStructures

      when( "arbitrary slices are taken" )
      then( "they should be the same to the Vector counterparts" )

      for( i <- 0 until N2 ) {
         val n = rnd.nextInt( finger.size + 1 )
         val (slicef, slices) = rnd.nextInt( 4 ) match {
            case 0 => (finger.take( n ), scala.take( n ))
            case 1 => (finger.drop( n ), scala.drop( n ))
            case 2 => (finger.takeRight( n ), scala.takeRight( n ))
            case 3 => (finger.dropRight( n ), scala.dropRight( n ))
         }

         assert( slicef.toList == slices.toList, "Elements not equal" )
         assert( slicef.size   == slices.size, "Reported size not equal" )
         assert( slicef.sum    == slices.map( _.toLong ).sum, "Reported sum not equal" )
      }
   }

   def scenarioWithTime( name: String, descr: String )( body: => Unit ) {
      scenario( descr ) {
         val t1 = System.currentTimeMillis()
         body
         val t2 = System.currentTimeMillis()
         println( "For " + name + " the tests took " + formatSeconds( (t2 - t1) * 0.001 ))
      }
   }

   def formatSeconds( seconds: Double ) : String = {
      val millisR    = (seconds * 1000).toInt
      val sb         = new StringBuilder( 10 )
      val secsR      = millisR / 1000
      val millis     = millisR % 1000
      val mins       = secsR / 60
      val secs       = secsR % 60
      if( mins > 0 ) {
         sb.append( mins )
         sb.append( ':' )
         if( secs < 10 ) {
            sb.append( '0' )
         }
      }
      sb.append( secs )
      sb.append( '.' )
      if( millis < 10 ) {
         sb.append( '0' )
      }
      if( millis < 100 ) {
         sb.append( '0' )
      }
      sb.append( millis )
      sb.append( 's' )
      sb.toString()
   }
}
