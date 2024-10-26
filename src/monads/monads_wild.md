![Updated][update-shield]

# Monads in the Wild

Monads are so ubiquitous in programming that most libraries (even in general-purpose programming languages) expose monads. For example, the ReactiveX library in Java, which provides facilities for reactive programming, exposes an `Observable` class, which is a monad. In addition, most stream processors in data streaming libraries (across many languages) are also monads. You will typically know when something is a monad if it has a method called `flatMap`, which is the same as `>>=` in Haskell.

Therefore, whenever you are defining your own libraries for your own needs, think about what behaviours your library should support:
- Does your library involve nondeterminism or streams/lists of data?
- Does your library perform I/O?
- Does your library produce potentially empty computation?
- Does your library potentially fail?
- Does your library read from an environment?
- Does your library write to additional state?
- Does your library process state?

If the answer to one (or more) of the questions above is yes, chances are, your library should expose a monad! Furthermore, if you are writing Haskell code, your library functions can likely be described as the composition of some of the commonly used monads provided in Haskell's Prelude, the `transformers` library, or the `mtl` library.

Give this a try in the exercises and the assignment!

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
