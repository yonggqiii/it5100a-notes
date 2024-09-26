![Updated][update-shield]
# Monads

Another incredibly useful tool is to be able to perform _composition in context_. That is, that given something of `f a` and a function from `a -> f b`, how do we get an `f b`?

Consider the following example. We can write 123 divided by 4 _and then_ divided by 5 via the following straightforward program:

```haskell
x, y, z :: Int
x = 123
y = (`div` 4) x
z = (`div` 5) y
```
However, we know that `div` is unsafe since dividing it by 0 gives a zero division error. Therefore, we should write a safe `div` function that returns `Nothing` if division by 0 is to be expected:

```haskell
safeDiv x y :: Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = div x y
```

However, composing `safeDiv` is now no longer straightforward:

```haskell
x = 123
y = (`safeDiv` 4) x
z = ???
```

```
     safeDiv                             safeDiv
        ┏━━━━                ?              ┏━━━━
Int ━━━━┫      Maybe Int  <----->   Int ━━━━┫     Maybe Int
        ┗━━━━                               ┗━━━━
```

Let us try using `fmap`:

```haskell
x :: Int
x = 123

y :: Maybe Int
y = (`safeDiv` 4) x

z :: Maybe (Maybe Int)
z = fmap (`safeDiv` 5) y
```

Although this typechecks, the resulting type `Maybe (Maybe Int)` is incredibly awkward. It tells us that there is potentially a `Maybe Int` term, which means that there is _potentially_ a _potential_ `Int`. What would be better is to collapse the `Maybe (Maybe Int)` into just `Maybe Int`.

For this, we introduce the notion of a _Monad_, which again, can be described by a typeclass with some rules governing their methods. The primary feature of a `Monad` `m` is that it is an `Applicative` where we can collapse an `m (m a)` into an `m a` in the most obvious way. However, for convenience's sake, Haskell defines the `Monad` typeclass in a slightly different (but otherwise equivalent) formulation[^1]:

```haskell
class Applicative m => Monad m where
    return :: a -> m a -- same as pure
    (>>=) :: m a -> (a -> m b) -> m b -- composition in context
```
These methods are governed by the following laws:
- Left identity: `return a >>= h` = `h a`
- Right identity: `m >>= return` = `m`
- Associativity: `(m >>= g) >>= h` = `m >>= (\x -> g x >>= h)`

`return` is practically the same as `pure` (in fact it is almost always defined as `return = pure`). Although the word `return` feels incredibly odd, we shall see very shortly why it was named this way. `>>=` is known as the _monadic bind_[^1] [^2], and allows us to perform computation in context on a term in context, thereby achieving _composition in context_. 

`>>=` is somewhat similar to `fmap`, in that while `fmap` allows us to apply an `a -> b` onto an `f a`, `>>=` allows us to apply an `a -> m b` onto an `m a`.


Let us see an instance of `Monad`:

```haskell
instance Monad Maybe where
    return :: a -> Maybe a
    return = pure

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f  = f x
```

With this instance, instead of using `fmap` to bring our `Maybe Int` into a `Maybe (Maybe Int)`, we can use `>>=` to just bring it to a `Maybe Int`!

```haskell
x :: Int
x = 123

y :: Maybe Int
y = (`safeDiv` 4) x

z :: Maybe Int
z = y >>= (`safeDiv` 5)
```

As we know, function composition `(g . f) x` is sort of to say "do `f` _and then_ do `g` on `x`". Similarly, when `f` and `g` are computations in context and `x` is a term in context, `x >>= f >>= g` also means "do `f` _and then_ do `g` on `x`"! However, `>>=` is incredibly powerful because the actual definition of `>>=` depends on the monad you use&mdash;therefore, monads allow us to _overload_ composition in context![^3]

```
     safeDiv   |                       safeDiv
        ┏━━━━  |                          ┏━━━━
Int ━━━━┫      | Maybe Int  >>=   Int ━━━━┫     Maybe Int
        ┗━━━━  |                          ┗━━━━
```
Therefore, if you had `f :: a -> b` and `g :: b -> c` and `x :: a`, you would write `g (f x)` for `f` _and then_ `g`. However, if you had `f :: a -> m b` and `g :: b -> m c` and `x :: m a`, you would write `x >>= f >>= g` for `f` _and then_ `g`.


## Beyond the Railways
As we know, data structures like `Maybe`, `Either` and `Validation` support the railway pattern, and them being functors, applicatives and (in the case of `Maybe` and `Either`) monads makes them ergonomic to use. However, the use of functors, applicatives and monads extend beyond just the railway pattern.

As described in [Chapter 4.1 (Context/Notions of Computation)](./context.md), types like `[]` and `IO` provide _context_ around a type. As it turns out, these types are also functors, applicatives and monads. While we have not touched `IO` at all so far, and will only do so in the next chapter, let us see the instance definitions for `[]`:

```haskell
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

instance Applicative [] where
    pure :: a -> [a]
    pure x    = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Monad []  where
    return :: a -> [a]
    return = pure

    (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y <- f x]
```
Observe the definition of `>>=` for lists. The idea is that whatever `fmap f xs` produces (which is a 2+D list), `xs >>= f` flattens that result (it doesn't flatten it recursively, just the top layer). It does so by applying `f` onto every single `x`s in the list. As per the type signature, each `f x` produces a term of the type `[b]`, which is a list. We extract each `y` from that list, and put them all as elements of the resulting list. Let us see the action of `>>=` through an example:
```haskell
ghci> fmap (\x -> return x) [1, 2, 3]
[[1], [2], [3]] -- fmap gives a 2D list
ghci> [1, 2, 3] >>= (\x -> return x)
[1, 2, 3]       -- >>= gives a 1D list

ghci> fmap (\x -> return (x, x + 1)) [1, 2, 3]
[[(1, 2)], [(2, 3)], [(3, 4)]] -- fmap gives a 2D list
ghci> [1, 2, 3] >>= (\x -> return (x, x + 1))
ghci> [(1, 2), (2, 3), (3, 4)] -- >>= gives a 1D list

ghci> [1, 2] >>= (\x -> [3] >>= (\y -> >>= return (x, y)))
[(1, 3), (2, 3)]
```
The last function can be written a little more clearly. Suppose we want to write a function that produces the "cartesian product" of two lists. Writing this function using the monad methods can look unwieldy, but will ultimately pay off as you will see shortly:

```haskell
cartesian_product :: [a] -> [a] -> [(a, a)]
cartesian_product xs ys = xs >>= (\x -> 
                          ys >>= (\y -> 
                          return (x, y)))
```
As we expect, everything works!

```haskell
ghci> cartesian_product [1,2] [3]
[(1,3),(2,3)]
```

## Do-notation
The definition of `cartesian_product` above is hard to read. However, this form of programming is (as you will surely see) very common&mdash;we bind each `x` from `xs`, then bind each `y` from `ys`, and return `(x, y)`. Why not let us write the same implementation in this way:

```haskell
cartesian_product :: [a] -> [a] -> [(a, a)]
cartesian_product xs ys = do
    x <- xs
    y <- ys
    return (x, y)
```
Wouldn't this be much more straightforward? In fact, Haskell supports this! This is known as `do` notation, and is supported as long as the expression's type is a monad. `do` notation is just syntactic sugar for a series of `>>=` and lambda expressions:
```
do e1 <- e2           ==>      e2 >>= (\e1 -> whatever code)
   whatever code
```
Therefore, the definition of `cartesian_product` using `do` notation is translated as follows:

```
do x <- xs                 xs >>= (\x ->              xs >>= (\x ->
   y <- ys           ==>      do y <- ys         ==>  ys >>= (\y ->
   return (x, y)                 return (x, y))       return (x, y)))
```
More importantly, go back to the definition of `cartesian_product` using `do` notation. Compare that definition with the (more-or-less) equivalent definition in Python:
```python
def cartesian_product(xs, ys):
    for x in xs:
        for y in ys:
            yield (x, y)
```
What we have done was to **recover imperative programming with do-notation**! Even better: while `for` loops in Python only work on iterables, `do` notation in Haskell works on **any monad**!

```haskell
cartesian_product :: Monad m => m a -> m b -> m (a, b)
cartesian_product xs ys = do
    x <- xs
    y <- ys
    return (x, y)
```

```haskell
ghci> cartesian_product [1, 2] [3]
[(1, 3), (2, 3)]
ghci> cartesian_product (Just 1) (Just 2)
Just (1, 2)
ghci> cartesian_product (Just 1) Nothing
Nothing
ghci> cartesian_product (Right 1) (Right 2)
Right (1, 2)
ghci> cartesian_product getLine getLine -- getLine is like input() in Python
alice -- user input
bob   -- user input
("alice","bob")
```
As you can tell, each monad has its own way of composing computation in context and has its own meaning behind the context it provides. This is why monads are such a powerful tool for functional programming! It is for this reason that we will dedicate the entirety of the next chapter to monads.

---

[^1]: You might notice that the monadic bind operator `>>=` looks very similar to the Haskell logo. Monads are incredibly important in functional programming, and we shall spend an entire chapter dedicated to this subject.
[^2]: Many popular languages call this `flatMap`.
[^3]: Just like how languages like C, C++ and Java have `;` to separate statements, i.e. a program like `A;B` means do `A` and then do `B`, `>>=` allows us to _overload_ what _and then_ means!

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20SEP%202024-57ffd8?style=for-the-badge
