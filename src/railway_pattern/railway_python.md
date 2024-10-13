![Updated][update-shield]
## Key Takeaways
- Instead of functions with side-effects, pure functions can emulate the desired effects (like branching railways) using the right data structures as notions of computation
- We can operate in context using regular functions when the context is a functor
- We can combine context when the context is an applicative
- We can compose functions in context sequentially when they are monads

# Railway Pattern in Python

Aside from `do`-notation and all the niceties of programming with typeclasses, nothing else we have discussed in this chapter is exclusive to Haskell. In fact, many other languages have similar data structures to the ones we have seen, and are all functors and monads too! For example, we can implement `safeDiv` in `Java` using the built-in `Optional` class, which is the same as `Maybe` in Haskell, and to use its `flatMap` method instead of `>>=` in Haskell:
```java
import java.util.Optional;
public class Main {
  static Optional<Integer> safeDiv(int num, int den) {
    if (den == 0) {
      return Optional.empty();
    }
    return Optional.of(num / den);
  }

  public static void main(String[] args) {
    Optional<Integer> x = safeDiv(123, 4)
        .flatMap(y -> safeDiv(y, 5))
        .flatMap(z -> safeDiv(z, 2));
    x.ifPresent(System.out::println);
  }
}
```

Therefore, what is required for using the railway pattern are 
- the right data structures that have happy/sad paths, just like `Maybe`, `Either` and `Validation` (or even `[]`)
- the right methods so that they are functors, applicatives, monads etc, ensuring that they adhere to the laws as derived from category theory
- idiomatic _uses_ of these data structures write pure functions, and to _use_ their methods to concisely express functorial, applicative or monadic actions

Give these a try in the exercises!


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-13%20OCT%202024-57ffd8?style=for-the-badge
