# Pattern Matching

We have seen how we can write constructors for algebraic data types, and
even use record syntax to create functions for accessing fields.
However, one natural question would then be to ask, how do we write
functions that access these fields, if we do not use record syntax? For
example, if we defined a fraction type normally, how do we obtain a
fraction's numerator and denominator?

The answer to this question is to use *pattern matching*. It is a
control structure just like if-else statements, except that we would
execute different branches based on the value/structure of the data,
instead of a general condition.

Let us define the factorial function using pattern matching instead of
conditional expressions or guards. We use `case` expressions
to do so:

``` haskell
fac :: Int -> Int
fac n = case n of -- match n against these patterns:
    0 -> 1
    x -> x * fac (x - 1) -- any other Int
```

The nice thing about pattern matching is that we can also match against
the *structure* of data, i.e. to match against constructors:

Let us
redefine the `fst` and `snd` functions which project
a tuple into its component values:

``` haskell
fst' :: (a, b) -> a
fst' p = case p of 
    (x, _) -> x

snd' :: (a, b) -> b
snd' p = case p of
    (_, y) -> y
```

Let us write accessor functions to access the numerator and denominator of a
fraction.

``` haskell
data Fraction = F Int Int
numerator, denominator :: Fraction -> Int
numerator f = case f of
    F x _ -> x
denominator f = case f of 
    F _ x -> x
```

One nice thing about Haskell is that because we perform pattern matching
over the arguments of functions so frequently, we can actually bring the
patterns up to the definitions of the functions themselves.

Let us define all of the functions we've just written using
`case` expressions into more idiomatic uses of
pattern matching.

``` haskell
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)
 
fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y
 
data Fraction = F Int Int
numerator, denominator :: Fraction -> Int

numerator (F x _) = x
denominator (F _ y) = y
```

We also know that the list type is a singly linked list, which is
roughly defined as such:

``` haskell
data [a] = [] | a : [a]
```

We can use this fact to pattern match against lists!

The sum of a list of integers is 0 if the list is empty, otherwise its
the head of the list plus the sum of the tail of the list.

``` haskell
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs
```
The length of a list is 0 if the list is empty, otherwise it is 1 more
than the length of its tail.

``` haskell
len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs
```

Really neat! Defining functions operating on algebraic data types
(including recursive data types) are very convenient thanks to pattern
matching! What's more, patterns can actually be used virtually anywhere
on the left side of any binding:

Let us use pattern matching in a `let` binding:

``` haskell
len :: [a] -> Int
len [] = 0
len ls = 
    let (_ : xs) = ls
    in  1 + len xs
```

Perhaps the most powerful feature of pattern matching is that the
compiler will warn you if your pattern matches are non-exhaustive, i.e.
if you do not match against all possible constructors of the type! Let
us define a function that only matches against the empty list
constructor.

``` haskell
-- Main.hs
emp :: [a] -> [a]
emp [] = []
```

Compile it to see the warning!

```output warn
ghc Main.hs
Main.hs:3:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for 'emp': Patterns of type '[a]' not matched: (_:_)
  |
3 | emp [] = []
  | ^^^^^^^^^^^
```

This is one reason why pattern matching is so powerful; compilers can
check if you have covered all possible patterns of a given type. This is
unlike usual if-else statements where it is much less straightforward to
check if you have covered all possible branches, especially if you omit
`else` statements.

One important point to highlight here is that pattern matching is done
top-down. Thus, using pattern-matching is kind of similar to if-else
statements in that regard: your most specific condition should be
defined first, then followed by more general or catch-all patterns.

The following factorial function is poorly defined, because the first
pattern match will match all possible integers, thereby causing the
function to never terminate:

``` haskell
fac :: Int -> Int
fac n = n * fac (n - 1)
fac 0 = 1 -- redundant as pattern above matches all
          -- possible integers
```

With pattern matching, let us know fulfil our earlier promise of
defining the `eval` function for the `Expr` GADT in
[Chapter 2.3 (Algebraic Data Types)](./algebraic_data_types.md).

In our Python formulation, we know that `eval` should have the
type signature `Expr a -> a`. Let us then define how each
expression should be evaluated with pattern matching.

``` haskell
eval :: Expr a -> a
eval (LitNumExpr n)   = n
eval (AddExpr a b)    = eval a + eval b
eval (EqExpr a b)     = eval a == eval b
eval (CondExpr a b c) = if eval a then eval b else eval c
```

This is highly straightforward! However, you might find that when this
program is compiled, the compiler throws an error on the use of the
`(==)` function:

```output error
ghc Main.hs
Main.hs:13:28: error:
    • Could not deduce (Eq a1) arising from a use of ‘==’
      from the context: a ~ Bool
        bound by a pattern with constructor:
                   EqExpr :: forall a. Expr a -> Expr a -> Expr Bool,
                 in an equation for ‘eval’
        at app/Main.hs:13:7-16
      Possible fix:
        add (Eq a1) to the context of the data constructor ‘EqExpr’
    • In the expression: eval a == eval b
      In an equation for ‘eval’: eval (EqExpr a b) = eval a == eval b
   |
13 | eval (EqExpr a b) = eval a == eval b
   |       
```

The reason for this is Haskell is unable to determine that the type
parameter `a` is amenable to equality comparisons. Solving this requires
an understanding of *typeclasses*, which we will explore in the next
lecture. For now, just include an `Eq a =>` constraint in our
GADT declaration.

You might also get a warning about pattern matching on GADTs being
fragile; that is because GADTs are actually a Haskell language
extension. As such, enable this extension when compiling this program,
or add a `LANGUAGE` *pragma* at the top of the file.

``` haskell
{-# LANGUAGE GADTs #-}
data Expr a where
  LitNumExpr ::         Int -> Expr Int
  AddExpr    ::         Expr Int -> Expr Int -> Expr Int
  EqExpr     :: Eq a => Expr a -> Expr a -> Expr Bool
  CondExpr   ::         Expr Bool -> Expr a -> Expr a -> Expr a
```

Our program should compile now!

### Pattern Matching in Python

Python also has pattern matching with `match` statements with
`case` clauses! It looks very similar to how we would write
`case` expressions in Haskell.

``` python
def factorial(n: int) -> int:
  match n:
    case 0: return 1
    case n: return n * factorial(n - 1)
```

We can also match on the structure of types by unpacking. For example,
defining a function that sums over a list of integers:

``` python
def sum(ls: list[int]) -> int:
  match ls:
    case []: return 0
    case (x, *xs): return x + sum(xs)
    case _: raise TypeError()
```

Alternatively, performing structural pattern matching over a so called
algebraic data type:

``` python
@dataclass
class Tree[a]: pass
 
@dataclass
class Node[a](Tree[a]):
    val: a
    left: Tree[a]
    right: Tree[a]
 
@dataclass
class Leaf[a](Tree[a]):
    val: a

def preorder[a](tree: Tree[a]) -> list[a]:
    match tree:
        case Node(v, l, r): return [v] + preorder(l) + preorder(r)
        case Leaf(v): return [v]
        case _: raise TypeError
```

However, notice that in the `sum` and `preorder` function definitions,
the last clause catches all patterns and raises an error. This is needed
to side-step the exhaustiveness checker. This is because we are using
classes to model algebraic data types, and Python does not always know
all the possible structures of a given class. In the case of `sum`,
Python's type system does not contain information about the length of a
list, so it has no way of determining exhaustiveness. In the case of
`preorder`, the reason omitting the last case gives a non-exhaustiveness
error is because we did not match against other possible subclasses of
`Tree`.

If we had formulated our `Tree` type using unions, pyright can determine
the exhaustiveness of our patterns:

``` python
type Tree[a] = Node[a] | Leaf[a]

@dataclass
class Node[a]:
    val: a
    left: Tree[a]
    right: Tree[a]

@dataclass
class Leaf[a]:
    val: a

def preorder[a](tree: Tree[a]) -> list[a]:
    match tree:
        case Node(v, l, r): return [v] + preorder(l) + preorder(r)
        case Leaf(v): return [v]
        # no need for further cases
```

However, this may not always be ideal, especially if we are to define
GADTs in Python. Until Algebraic Data Types or ways to annotate the
exhaustivity of subclasses (such as defining a *sealed* class) are
formally introduced, exhaustive pattern matching checks are going to be
difficult to do. When doing pattern matching in Python, ensure that all
possible cases are handled before doing a catch-all clause in your
`match` statement.

All-in-all, we have just introduced a new control structure known as
pattern matching. When should we use this control structure? The general
rule of thumb is as follows:

-   If you are doing different things based on the value and/or
    structure of data, use pattern matching. You can tell this is the
    case if you are doing equality and `isinstance` checks in
    your conditional statements in Python.

-   Otherwise, you are likely going with the more general case of doing
    different things based on the satisfiability of a condition, in
    which case, rely on if-else statements, or in Haskell, conditional
    expressions and/or guards.


[^4]: The code snippet requires Python 3.12.
