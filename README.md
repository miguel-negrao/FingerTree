## FingerTree

### statement

This provides a data structure called FingerTree[^1] for the Scala programming language. The code was basically extracted from the FingerTree class from [Scalaz](http://code.google.com/p/scalaz/), stripping away as much of the encumbered Scalaz as possible, so that it becomes a halfway self contained project. It is also an attempt to make a more standard Scala like interface, including ordered sequences and interval trees.

All modifications by Hanns Holger Rutz to the original code published under the same BSD style license as Scalaz. To contact the author, send an email to `contact at sciss.de`.

[^1] Hinze, R. and Paterson, R., Finger trees: a simple general-purpose data structure, Journal of Functional Programming, vol. 16 no. 2 (2006), pp. 197--217

### notes

Currently the FingerTree, used for indexed sequences, is around 20 times slower than Scala's standard Vector class. Main problem I guess is missing specialization...

### building

This builds with Scala 2.9.1 and sbt 0.11.2.

### creating an IntelliJ IDEA project

To develop the sources of FingerTree in IntelliJ IDEA, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "FingerTree"
    > gen-idea

### remarks

Daniel Spiewak has an alternative implementation, which we have forked at [github.com/Sciss/scala-collections](https://github.com/Sciss/scala-collections), and we are currently benchmarking both implementations.

Spiewak's implementation [CC] is far more slim (and probably less complete) than Scalaz' [Z], so it comes at no surprise that it is faster in the preliminary tests: Constructing a tree by successive prepending for N = 50000 takes 13 ms in [CC] versus 288 ms in [Z]. For N = 100000, [CC] takes 22 ms versus stack overflow in [Z].

So the plan is to add the stuff we need (which is the measure to construct ordered, indexed, and summed sequences) to [CC] and give up the [Z] path.
