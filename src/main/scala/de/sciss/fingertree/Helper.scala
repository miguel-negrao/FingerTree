package de.sciss.fingertree

object Helper extends Reducers /* with Endos */ {
   // HH from Zeroes
   def mzero[Z](implicit z: Zero[Z]): Z = z.zero

   // HH from Identities
   implicit def IdentityTo[A](x: => A): Identity[A] = new Identity[A] {
     lazy val value = x
   }

   // HH from Options
   implicit def OptionTo[A](o: Option[A]): OptionW[A] = new OptionW[A] {
     val value = o
   }

   // HH from Zeroes
   def zero[Z](z: Z): Zero[Z] = new Zero[Z] {
     val zero = z
   }

   // HH from Semigroups
   def semigroup[S](f: (S, => S) => S): Semigroup[S] = new Semigroup[S] {
     def append(s1: S, s2: => S) = f(s1, s2)
   }

//   // MAsLow
//   implicit def maImplicit[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
//     val value = a
//   }
}