## FingerTree

### statement

FingerTree is an immutable sequence data structure in Scala programming language, offering O(1) prepend and append, as well as a range of other useful properties [^1]. Finger trees can be used as building blocks for queues, double-ended queues, priority queues, indexed and summed sequences.

FingerTree is (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](https://raw.github.com/Sciss/FingerTree/master/licenses/FingerTree-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

The current implementation is a rewrite of previous versions. It tries to combine the advantages of the finger tree found in Scalaz (mainly the ability to have reducers / measures) and of the finger tree implementation by Daniel Spiewak (small, self-contained, much simpler and faster), but also has a more idiomatic Scala interface and comes with a range of useful applications, such as indexed and summed sequences.

[^1] Hinze, R. and Paterson, R., Finger trees: a simple general-purpose data structure, Journal of Functional Programming, vol. 16 no. 2 (2006), pp. 197--217

### building

This builds with Scala 2.9.1 and sbt 0.11.2. Standard targets are `compile`, `package`, `doc`, `console`, `test`, `publish-local`.

### using

You can either implement your own data structure by wrapping a plain `FingerTree` instance. Trait `FingerTreeLike` can be used as a basis, it has two abstract methods `tree` and `wrap` which would need to be implemented.

Or you can use any of the provided ready-made data structures, such as `IndexedSeq` or `IndexedSummedSeq`. While the former might not be particularly interesting, as it does not add any functionality that is not found already in Scala's own immutable `IndexedSeq` (i.e. `Vector`), the latter provides the additional feature of measuring not just the indexed positions of the tree elements, but also an accumulative "sum" of any sort.

The core element for new structures is to provide an instance of `Measure` which is used by the finger tree to calculate the annotated meta data of the elements. The measure provdes a `zero` value, a `unit` method which measures exactly one element, and a summation method `|+|` which accumulates measured data. To work correctly with the caching mechanism of the finger tree, `|+|` must be associative, i.e. `(a |+| b) |+| c = a |+| (b |+| c)`.

Future versions will provide more ready-made structures, such as ordered sequences and interval sequences. In the meantime, you can check out the previous Scalaz based version of this project at git tag `Scalaz`, which includes those structures.

### todo

 - concatenation of trees
 - efficient bulk loading
 - the iterator looks broken (non-lazy)

### creating an IntelliJ IDEA project

To develop the sources of FingerTree in IntelliJ IDEA, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "FingerTree"
    > gen-idea
