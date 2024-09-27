![Updated][update-shield]

# Commonly-Used Typeclasses

Let us have a look at some typeclasses and their methods that you have
already used.

``` haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer a
```

## Equality Comparisons

The `Eq` typeclass describes types that are amenable to
equality comparisons; the `Num` typeclass describes types that
can behave as numbers, with support for typical numeric operations like
addition, subtraction and so on. Haskell's Prelude already ships with
the instances of these typeclasses for commonly-used types, such as
instances for `Num Int` and `Eq String`.

Let us try defining our own instance of `Eq`. Suppose we are
re-using the `Fraction` algebraic data type defined in [Chapter 2.3 (Types#Algebraic Data Types)](../../types/sections/algebraic_data_types.md):

``` haskell
data Fraction = Fraction Int Int
```

We allow `Fraction` to be amenable to equality comparisons
by implementing a typeclass instance for `Eq Fraction`:

``` haskell
instance Eq Fraction where
  (==) :: Fraction -> Fraction -> Bool
  F a b == F c d = a == c && b == d

  (/=) :: Fraction -> Fraction -> Bool
  F a b /= F c d = a /= c || b /= d
```

Firstly, notice that we are performing equality comparisons between the
numerators and denominators. This is okay because we know that the
numerators and denominators of fractions are integers, and there is
already an instance of `Eq Int`. Next, usually by definition,
`a /= b` is the same as `not (a == b)`. Therefore,
having to always define both `(==)` and `(/=)` for
every instance is cumbersome.

## Minimal Instance Definitions

Let us inspect the definition of the `Eq` typeclass:

``` haskell
ghci> :i Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
    -- Defined in 'GHC.Classes'
```

Notice the `MINIMAL` *pragma*&mdash;the pragma states that we only need to
define either `(==)` *or* `(/=)` for a complete
instance definition! Therefore, we can omit the definition of
`(/=)` in our `Eq Fraction` instance, and we would
still have a complete definition:

``` haskell
instance Eq Fraction where
  (==) :: Fraction -> Fraction -> Bool
  F a b == F c d = a == c && b == d
```

``` haskell
ghci> Fraction 1 2 == Fraction 1 2
True
ghci> Fraction 1 2 /= Fraction 1 2
False
```

A natural question to ask is, why not simply define `Eq` to
only have `(==)` and give `(/=)` for free?

``` haskell
class Eq a where
  (==) :: a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
x /= y = not (x == y)
```

By placing both functions as methods in the typeclass, programmers have
the option to define *either* `(==)` *or* `(/=)`, or
both, if specifying each implementation individually gives better performance or 
different behaviour than the default.

## Typeclass Constraints in Typeclasses and Instances

We can even define instances over polymorphic types. Here is an example
of how we can perform equality comparisons over trees:

``` haskell
data Tree a = Node (Tree a) a (Tree a)
            | Empty

instance Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  (Node l v r) == (Node l' v' r') = l == l' && v == v' && r == r'
  _ == _ = False
```

However, our instance will not type-check because the elements
`a` of the trees also need to be amenable to equality
comparisons for us to compare trees! Therefore, we should constrain
`a` with `Eq` in the *instance* declaration, like
so:

``` haskell
data Tree a = Node (Tree a) a (Tree a)
            | Empty

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  (Node l v r) == (Node l' v' r') = l == l' && v == v' && r == r'
  _ == _ = False
```

In fact, we can write typeclass constraints in typeclass declarations as
well. For example, the `Ord` typeclass describes (total)
orders on types, and all (totally) ordered types must also be amenable
to equality comparisons:

``` haskell
class Eq a => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  -- ...
```

## Deriving Typeclasses

In fact, some typeclasses are so straightforward that defining instances
of these classes are a tedium. For example, the `Eq` class is
(usually) very straightforward to define&mdash;two terms are equal if they
are built with the same constructor and their argument terms are
respectively equal. As such, the language should not require programmers
to implement straightforward instances of classes like `Eq`.

Haskell has a *deriving mechanism* that allows the compiler to
automatically synthesize typeclass instances for us. It is able to do so
for `Eq`, `Ord`, and others like `Enum`.
Doing so is incredibly straightforward:

``` haskell
data A = B | C

data Fraction = Fraction Int Int 
  deriving Eq -- deriving Eq Fraction instance

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Eq, Show) -- deriving Eq (Tree a) and Show (Tree a)

deriving instance Eq A -- stand-alone deriving declaration
```

These declarations tell the compiler to synthesize instance declarations
in the most obvious way. This way, we do not have to write our own
instance declarations for these typeclasses!

``` haskell
ghci> x = Node Empty 1 Empty
ghci> y = Node (Node Empty 1 Empty) 2 Empty
ghci> x
Node Empty 1 Empty
ghci> x == y
False
```


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
