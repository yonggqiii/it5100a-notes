![Updated][update-shield]

# Functional Dependencies

Observe the type of `(+)`:

``` haskell
:t (+)
(+) :: forall a. Num a => a -> a
```

This is quite different in Python:

```python-repl
>>> type(1 + 1)
class <'int'>
>>> type(1 + 1.0)
class <'float'>
>>> type(1.0 + 1)
class <'float'>
>>> type(1.0 + 1.0)
class <'float'>
```

The `+` operator in Python behaves heterogenously&mdash;when given
two `int`s we get an `int`; when given at least one
`float` we get a `float`. How would we encode this in
Haskell?

Simple! Create a multi-parameter typeclass that describes the argument
types and the result type!

``` haskell
class (Num a, Num b, Num c) => HAdd a b c where
  (+#) :: a -> b -> c
```

Then we can write instances for the possible permutations of the desired
types:

``` haskell
instance Num a => HAdd a a a where
  (+#) :: a -> a -> a
  (+#) = (+)

instance HAdd Int Double Double where
  (+#) :: Int -> Double -> Double
  x +# y = fromIntegral x + y

instance HAdd Double Int Double where
  (+#) :: Double -> Int -> Double
  x +# y = x + fromIntegral y
```

However, trying to use `(+#)` is very cumbersome:

``` haskell
ghci> x :: Int = 1
ghci> y :: Double = 2.0
ghci> x +# y
<interactive>:3:1: error:
    - No instance for (HAdd Int Double ()) arising from a use of 'it'
    - In the first argument of 'print', namely 'it'
      In a stmt of an interactive GHCi command: print it
ghci> x +# y :: Double
3.0
```

This occurs because without specifying the return type `c`,
Haskell has no idea what it is since it is ambiguous! As per the
definition, no one is stopping us from defining another
`instance HAdd Int Double String`! On the other hand, we know
that adding an `Int` and a `Double` *must* result in
a `Double` and nothing else; in other words, the types of the
arguments to `(+#)` *uniquely characterizes* the resulting
type.

The way we introduce this dependency between these type variables by
introducing *functional dependencies* on typeclass declarations, which,
adding them to our declaration of `HAdd`, looks something like
the following:

``` haskell
{-# LANGUAGE FunctionalDependencies #-}
class (Num a, Num b, Num c) => HAdd a b c | a b -> c where
  (+#) :: a -> b -> c
```

The way to read the clause `a b -> c` is "`a` and `b` *uniquely
characterizes*/*determines* `c`", or in other words, `c` is a *function*
of `a` and `b`, i.e. it is not possible that given a *fixed* `a` and `b`
that we have two different inhabitants of `c`. This (1) prevents the
programmer from introducing different values of `c` for the same `a` and
`b` (which we haven't) and (2) allows the compiler to infer the right
instance just with `a` and `b` alone.

``` haskell
ghci> x :: Int = 1
ghci> y :: Double = 2.0
ghci> x +# y
3.0
ghci> :{
ghci| instance HAdd Int Double String where
ghci|   x +# y = show x
ghci| :}
<interactive>:8:10: error:
    Functional dependencies conflict between instance declarations:
      instance [safe] HAdd Int Double Double
        -- Defined at <interactive>:17:10
      instance HAdd Int Double String -- Defined at <interactive>:21:10
```

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
