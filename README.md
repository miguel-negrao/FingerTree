## FingerTree

### statement

FingerTree is an immutable sequence data structure in Scala programming language, offering O(1) prepend and append, as well as a range of other useful properties [^1]. Finger trees can be used as building blocks for queues, double-ended queues, priority queues, indexed and summed sequences.

FingerTree is (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](https://raw.github.com/Sciss/FingerTree/master/licenses/FingerTree-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

The current implementation is a rewrite of previous versions. It tries to combine the advantages of the finger tree found in Scalaz (mainly the ability to have reducers / measures) and of the finger tree implementation by Daniel Spiewak (small, self-contained, much simpler and faster), but also has a more idiomatic Scala interface and comes with a range of useful applications, such as indexed and summed sequences.

[^1] Hinze, R. and Paterson, R., Finger trees: a simple general-purpose data structure, Journal of Functional Programming, vol. 16 no. 2 (2006), pp. 197--217

### building

This builds with Scala 2.9.1 and sbt 0.11.2. Standard targets are `compile`, `package`, `doc`, `console`, `test`, `publish-local`.

### creating an IntelliJ IDEA project

To develop the sources of FingerTree in IntelliJ IDEA, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "FingerTree"
    > gen-idea
