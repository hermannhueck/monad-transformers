# From Functor Composition to Monad Transformers

Code and slides for my talk on this subject.

In a List[Option[A]] or Future[Option[A]] we want to access the value of type A
conveniently without nested mapping and flatMapping.

We can avoid nested mapping with composed Functors and composed Applicatives.

Functors compose, Applicatives compose, but Monads do not!
  
What can we do?

Monad transformers to the rescue!

After going through the composition of Functors and Applicatives
I show how 2 Monads are bolted together with
a Monad transformer and how to use this construct. I demonstrate this with the
Option transformer OptionT and will end up with best practices.

