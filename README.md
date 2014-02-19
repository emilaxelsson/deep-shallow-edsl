deep-shallow-edsl
=================

Slightly adapted code from the paper "Combining Deep and Shallow Embedding for EDSL" (published in Trends in Functional Programming TFP 2012):

  * <http://www.cse.chalmers.se/~emax/documents/svenningsson2013combining.pdf>
  * <http://dx.doi.org/10.1007/978-3-642-40447-4_2>

The paper descibes a design pattern for embedded DSLs (EDSLs) where a combination of deep and shallow embedding is used in order to get most advantages from both approaches.

In short, the idea is as follows:

  * The deep embedding `FunC` is designed with respect to the code we want to generate, not what we want the user to write.
  * High-level libraries (e.g. `Vector` and its operations) are implemented as shallow embeddings on top of `FunC`.
  * The `Syntactic` class is used to connect the deep and the shallow embeddings.

The approach has several advantages:

  * **Simplicity**    -- Moving functionality to shallow embeddings helps keep the AST small without sacrificing expressiveness.
  * **Extensibility** -- New shallow embeddings can often be added without changing the deep embedding, or by just changing is slightly.
  * **Abstraction**   -- The shallow embeddings are based on /abstract data types/ leading to better programming interfaces (more like ordinary APIs than constructs of a language). This has important side-effects:
      - The shallow interfaces can have properties not possessed by the deep embedding. For example, the `Vector` interface guarantees removal of intermediate structures.
      - The abstract types can sometimes be made instances of standard Haskell type classes, such as `Functor` and `Monad` (see the `Option` and `Vector` types), even when the deep embedding cannot.

This style of EDSL design is supported by the Syntactic library:

  * <http://hackage.haskell.org/package/syntactic>

Syntactic has generic functionality that gives some of the things defined in this module for free.

