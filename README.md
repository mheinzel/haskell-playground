Some things I played around with to understand them better. Fun stuff!

- **monads**:

  Simple implementation of the Reader monad, followed by a monad instance for functions

- **catamorphisms** (also called generic eliminators or recursion schemes):

  Catamorphism deconstruct a data structure and are a generalization of the concept of a fold.
  Instead of writing a recursive function on the data structure manually, you just pass the
  respective algebra (functions that describe how to deconstruct each constructor) to a generic
  eliminator.

  Thanks to Andor Pénzes for exposing me to the idea! More information can be found on the
  [Haskell Wiki](https://wiki.haskell.org/Catamorphisms).

- **epsilon**:

  A seemingly impossible program: It checks whether total predicates on infinite lists of Bools are

  - satisfiable (returning an example)
  - a tautology
  - equal to another predicate

  despite there being uncountably many of these lists.

  Thanks to Ingo Blechschmidt for exposing me to the idea! Extensive information can be found in
  this [blog post](http://math.andrej.com/2008/09/28/seemingly-impossible-functional-programs/)
  by Martín Escardó.
