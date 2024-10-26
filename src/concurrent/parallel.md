![Updated][update-shield]

# Parallel Programming
Let us now turn our focus to parallel programming. For many large problems, we could divide them into chunks and evaluate the solution for these chunks at the same time on multiple cores, before combining the results, just like a divide-and-conquer approach. However, doing so is traditionally seen as difficult, and we usually use the same libraries and language primitives that are used for concurrency to develop a parallel program. Writing parallel programs in general-purpose imperative languages can be complex and tedious.

While we could certainly use Haskell’s concurrency features like `forkIO`, `MVar` and `Chan` to develop parallel code, there is a much simpler approach available to us. All we need to do is to annotate some sub-expressions in our functions to make them evaluated in parallel.

## Non-Strict Evaluation
In the very beginning of this course, we described Haskell as a non-strict evaluation language. That is, Haskell decides the evaluation strategy for us, unlike other strict evaluation languages where things are evaluated in a deterministic and specific format. For example, in Python, a function call is evaluated by first fully evaluating its arguments, then executing each statement in the function from top down. Haskell generally only evaluates terms _by need_, giving rise to a notion of _lazy evaluation_. 

The key idea of attaining _parallelism_ in Haskell is by specifying _parallel evaluation strategies_. 

### Strict Evaluation
Before we bgin describing how to evaluate terms in parallel, we must first describe how we can even force the evaluation of a term in the first place. For example, in the following program:
```haskell
ghci> x = [1..]
ghci> y = sum x
```
virtually nothing is evaluated, and GHCI does not enter an infinite loop. This is because there is as yet no demand for the evaluation of `y`. Of course, if we attempt to evaluate `y`, we do arrive at an infinite loop, because evaluating the actual sum of `x` is required to determine what `y` is.

Therefore, whenever an expression is encountered, Haskell allocates a _thunk_ as a uncomputed placeholder for the result of the expression evaluation. The thunk is only evaluated by need (usually as little as possible) to evaluate other parts of code.

For example:

```haskell
ghci> x = [1..]
ghci> case x of { [] -> 0; (x:xs) -> x }
1
```
Notice that the `case` expression demands the evaluation of `x`. However, it does not demand the _complete_ evaluation of `x`. Instead, it only demands to know the constructor of `x`. Therefore, when executing `x = [1..]`, Haskell puts a completely unevaluated thunk, for `x`, and the `case` expression then evaluates `x` to _head normal form_ (HNF) (evaluating to the constructor but not its arguments)[^1] to perform the case analysis.

Another example of lazy evaluation is with `let` expressions:

```haskell
ghci> let x = [1..]; y = sum x in 1 + 2
3
```

Again, Haskell does not evaluate `y` at all since it is not demanded in the evaluation of `1 + 2`!

This may be a problem for concurrency and parallelism, because it is possible for `forkIO` to push an I/O action to a different thread, only for that thread to allocate an unevaluated thunk for it, and when its evaluation is demanded, the evaluation is done on the main thread!

```haskell
expensive :: MVar String -> IO ()
expensive var = do
    putMVar var expensivelyComputedString

main :: IO ()
main = do
    var <- newEmptyMVar
    forkIO $ expensive var
    whatever
    result <- takeMVar var
    print result
```

The program above gives the impression that the expensive computation is done on the forked thread. However, in reality, what could happen is that the thread running `expensive` only allocates a thunk for `expensivelyComputedString`, and returns. Then, when the `result` is demanded in the `main` I/O action running in the main thread, it is the main thread that computes the expensively computed string, thereby, achieving nothing from the concurrency.

It is for this reason that Haskell exposes primitives for deciding the evaluation of expressions. The one most used is `seq`, which introduces an artificial demand for an expression to be evaluated to head normal form:

```haskell
ghci> :t seq
seq :: a -> b -> b
```
The expression ``x `seq` y`` evaluates to `y`, but creates an artificial demand for the evaluation of `x` as well. Therefore, evaluating the following expression does not terminate:

```haskell
ghci> let x = [1..]; y = sum x in y `seq` 1 + 2
```
However, notice that the following _does_ terminate:

```haskell
ghci> let x = [1..] in x `seq` 1 + 2
3
```

This is because `seq` only creates an artificial demand for `x` to be evaluated to _head normal form_, i.e. up to the evaluation of its constructor.

What we can do instead is to introduce a new _evaluation strategy_ for forcing the full evaluation of a list:
```haskell
ghci> :{
ghci| deepSeq :: [a] -> b -> b
ghci| deepSeq [] x = x
ghci| deepSeq (x:xs) y = x `seq` deepSeq xs y
ghci| :}
ghci> x = [1..]
ghci> x `seq` 1
1
ghci> x `deepSeq` 1
```

Using `deepSeq` now forces the full evaluation of `x`, which obviously does not terminate because `x` is infinitely large! However, note that `deepSeq` only evaluates the _elements_ to HNF&mdash;therefore, if `x` were a, for example, a two-dimensional list, the individual one-dimensional lists in `x` are only evaluated to HNF, i.e. only their constructors are evaluated.

## Parallel Evaluation
Since parallel programming is all about deciding what expressions to evaluate in parallel, all we need is some primitives that tell the compiler to evaluate an expression in parallel, just like `seq`! The `GHC.Conc` module exposes two evaluation primitives, `par` and `pseq` that allows us to do parallel programming easily:

```haskell
ghci> import GHC.Conc
ghci> :t par
par :: a -> b -> b
ghci> :t pseq
pseq :: a -> b -> b
```

`par` is straightforward to understand: ``x `par` y`` is an expression stating that there is an artificial demand for `x` that _could_ be evaluated to HNF in _parallel_. However, `par` does not _guarantee_ the parallel evaluation of `x`. This is because `x` could be a cheap computation that does not need to be, and should not be, evaluated in parallel, or that there are not enough cores available for the parallel evaluation of `x`.

Then, what is `pseq` for? Notice this: in an expression ``x `par` f x y``, we claim to want to evaluate `x` in parallel to HNF, _and then_ combine it with `y` using `f` in the current thread. However, this requires a guarantee that `y` is evaluated on the current thread _before_ the current thread attempts to evaluate `x`. Otherwise, it could be that `par` will queue a spark for the evaluation of `x`, and before a new thread can be sparked for that evaluation, the current thread evaluates `f x y`, which performs the evaluation of `x` first; therefore, no parallel evaluation of `x` happens, defeating of `par` in the first place.

Therefore, we need some primitive that performs the evaluation of an expression to HNF before another expression. `seq` does not do this; ``x `seq` y`` only claims to evaluate `x` to HNF, but does not enforce that to happen _before_ `y`. In contrast, `pseq` does. ``x `pseq` y`` guarantees that the evaluation of `x` to HNF happens _before_ the evaluation of `y`.

As such, `par` and `pseq` allow us to annotate computations with evaluation strategies to describe what computation happens in parallel, and what that computation is _in parallel with_. For example, the expression ``x `par` (y `pseq` f x y)`` states roughly that `x` happens in parallel with `y`, then the results are combined using `f`.

For example, let us try writing a parallel (but still exponential) fibonacci:

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = n1 `par` (n2 `pseq` (n1 + n2))
  where n1 = fib (n - 1)
        n2 = fib (n - 2)
```
Aside from the usual base cases, the recursive case computes the \\(n-1\\) and \\(n - 2\\) fibonacci numbers in parallel, then combines them together with addition. Described in words, the recursive case computes `fib (n - 1)` in parallel with `fib (n - 2)` by queueing a spark for `fib (n - 1)` and evaluating `fib (n - 2)` in the current thread, then adds the results together with plain addition.

Computing `fib 45` shows that for large values, having more cores makes a big difference.

```output info
> time cabal run playground -- +RTS -N20
Number of cores: 20
1134903170

________________________________________________________
Executed in    3.29 secs    fish           external
   usr time   53.14 secs  319.00 micros   53.14 secs
   sys time    0.47 secs  129.00 micros    0.47 secs

```

```output info
> time cabal run playground -- +RTS -N1
Number of cores: 1
1134903170

________________________________________________________
Executed in   12.93 secs    fish           external
   usr time   12.61 secs  418.00 micros   12.61 secs
   sys time    0.08 secs  171.00 micros    0.08 secs
```

## When Should We Parallelize?
However, notice the `usr` time for the case of running our program on 20 cores. Clearly, the CPU does more than 4x more work than the single core case; it just so happens that leveraging more cores makes the speed-ups outweigh the additional overhead. Indeed, while `par` is cheap, it is not _free_. Although Haskell threads are lightweight, threads in general will always incur some additional overhead, and at some point, the benefits of computing something in parallel are outweighed by the overhead of spawning a new thread for its computation. For example, in the case of computing `fib 3`, it is frankly completely unnecessary to compute `fib 2` and `fib 1` in parallel, since both are such small computations that run incredibly quickly.

Let us amend our implementation to only use parallelism for larger values. Smaller values are computed sequentially:

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
-- sequential for small n
fib n | n <= 10 = fib (n - 1) + fib (n - 2)
-- parallel for large n
fib n = n1 `par` (n2 `pseq` (n1 + n2))
  where n1 = fib (n - 1)
        n2 = fib (n - 2)
```

The execution time shows a significant speed-up on both the single core and multicore runtimes!

```output info
> time cabal run playground -- +RTS -N20
Number of cores: 20
1134903170

________________________________________________________
Executed in  892.37 millis    fish           external
   usr time   13.01 secs    646.00 micros   13.00 secs
   sys time    0.18 secs      0.00 micros    0.18 secs


```

```output info
> time cabal run playground -- +RTS -N1
Number of cores: 1
1134903170

________________________________________________________
Executed in    6.81 secs    fish           external
   usr time    6.71 secs  453.00 micros    6.71 secs
   sys time    0.03 secs    0.00 micros    0.03 secs
```

Generally speaking, knowing when to parallelize is a matter of experimentation, trial-and-error and engineering experience. It highly depends on the computation you are trying to parallelize, the kind of computation you are doing, the usual inputs to the computation, and so on.

## Parallel Strategies
Let us try writing a parallel mergesort:

```haskell
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort ls
  | n < 100 = merge left' right'
  | otherwise = par left' $ pseq right' $ merge left' right'
    where n = length ls `div` 2
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y : ys)
            | otherwise = y : merge (x:xs) ys
          (left, right) = splitAt n ls
          left' = mergesort left
          right' = mergesort right
```

Our `mergesort` function does a typical merge sort, except from the fact that we are using an immutable list. Let us write a supporting `main` function to test our program:

```haskell
main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn $ "Number of cores: " ++ show n
  let ls :: [Int] = [10000000, 9999999..1]
      ls' = mergesort ls
  print $ length ls'
```

```output info
> time cabal run playground -- +RTS -N20
Number of cores: 20
10000000

________________________________________________________
Executed in    3.58 secs    fish           external
   usr time   16.02 secs  381.00 micros   16.02 secs
   sys time    1.39 secs  159.00 micros    1.39 secs
```

```output info
> time cabal run playground -- +RTS -N1
Number of cores: 1
10000000

________________________________________________________
Executed in    6.11 secs    fish           external
   usr time    5.62 secs    0.00 micros    5.62 secs
   sys time    0.43 secs  586.00 micros    0.43 secs
```

From before, recall that because Haskell is a lazy language, it may be the case that not all the supposedly parallel computation happens in the other thread. Since both `par` and `pseq` evaluate their first arguments only to HNF, it really only does evaluation up until it determines the constructor of the list after sorting, leaving the remainder of the list unevaluated. Then, in `main`, when we obtain the `length` of the list, the main thread may then have to evaluate the remainder of the list in the same thread. Let us extract some more performance out of our parallel evaluation by actually evaluating everything deeply in the parallel computation using `deepSeq` from before:

```haskell
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort ls
  | n < 100 = merge left' right'
  | otherwise = par (deepSeq left') $ pseq right' $ merge left' right'
    where n = length ls `div` 2
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y : ys)
            | otherwise = y : merge (x:xs) ys
          (left, right) = splitAt n ls
          left' = mergesort left
          right' = mergesort right

deepSeq :: [a] -> ()
deepSeq [] = ()
deepSeq (x:xs) = x `seq` deepSeq xs
```

Now we should notice some more performance gains!

```output info
> time cabal run playground -- +RTS -N20
Number of cores: 20
10000000

________________________________________________________
Executed in    2.89 secs    fish           external
   usr time   18.04 secs  365.00 micros   18.04 secs
   sys time    0.68 secs  145.00 micros    0.67 secs
```

```output info
> time cabal run playground -- +RTS -N1
Number of cores: 1
10000000

________________________________________________________
Executed in    6.18 secs    fish           external
   usr time    5.59 secs  362.00 micros    5.59 secs
   sys time    0.46 secs  145.00 micros    0.46 secs
```

Some very smart people have also come up with nice and elegant ways to write parallel code. For example, using the [`parallel`](https://hackage.haskell.org/package/parallel) library, we can express parallel programs with `Strategy`'s in the `Eval` monad:

```haskell
mergesort :: (Ord a, NFData a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort ls
  | n < 100 = merge left' right'
  | otherwise = runEval $ do
      l <- rparWith rdeepseq left'
      r <- rseq right'
      return $ merge l r
  where n = length ls `div` 2
        (left, right) = splitAt n ls
        left' = mergesort left
        right' = mergesort right
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | x <= y = x : merge xs (y : ys)
          | otherwise = y : merge (x:xs) ys
```

Strategies also allow us to separate algorithm from evaluation. For example, we can write a parallel fibonacci like so:

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n | n <= 10 = fib (n - 1) + fib (n - 2)
fib n = runEval $ do 
    n1 <- rpar (fib (n - 1))
    n2 <- rseq (fib (n - 2))
    return $ n1 + n2
```

Alternatively, we can make clear the distinction between the underlying algorithm and the evaluation strategy with `using`:

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n | n <= 10   = n1 + n2
      | otherwise = (n1 + n2) `using` strat
  where n1 = fib (n - 1)
        n2 = fib (n - 2)
        strat v = do { rpar n1; rseq n2; return v }
```


We will leave it up to you to learn more about parallel Haskell with the [`parallel`](https://hackage.haskell.org/package/parallel) library. For more information, you may read the paper by [Marlow et al.; 2010](#seqnomore) that describes it. We shall not cover these because they, along with `par` and `pseq`, are much more Haskell-specific and less applicable to code written in general-purpose languages. The only goal of this chapter, which we hope has been achieved, is to show how easy it is to introduce parallelism to regular sequential programs in a purely functional programming language.

---
[^1]: Usually expressions are evaluated to _weak head normal form_ (WHNF), although the distinction is not crucial for our understanding.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge

## References

<a id="seqnomore" class="cite">
Simon Marlow, Patrick Maier, Hans-Wolfgang Loidl, Mustafa K. Aswad, and Phil Trinder. 2010. <code>seq</code> No More: Better Strategies for Parallel Haskell. In <i>Proceedings of the third ACM Haskell symposium on Haskell (Haskell '10)</i>. Association for Computing Machinery, New York, NY, USA, 91–102. <a class="cite" href="https://doi.org/10.1145/1863523.1863535">https://doi.org/10.1145/1863523.1863535</a>.
</a>
