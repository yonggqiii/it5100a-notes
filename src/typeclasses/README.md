Two of the most important aspects of software engineering design is _decoupling_ and _extensibility_, reducing the dependencies between two systems or programming constructs and making it easy to extend implementations. These are not simple problems for programming language designers to solve. Different languages offer different solutions to this problem, and some languages make these not easy.

In this chapter, we discuss how Haskell allows us to decouple types and functions, and in some sense, making data types extensible, without compromising on type-safety. Haskell does so with a programming feature not common to many languages, known as typeclasses.



