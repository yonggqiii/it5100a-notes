![Updated][update-shield]
# Exercises

### Question 1
Without using GHCI, determine the types of the following expressions:
1.  `1 + 2 * 3`
2.  `(show . )`
3.  `( . show)`
4.  `\ (a, b) -> a == b`

### Question 2
You are given the following untyped
program:

``` python
type Tree[a] = Empty | TreeNode[a]
type List[a] = Empty | ListNode[a]

@dataclass
class Empty:
    def to_list(self):
        return []

@dataclass
class ListNode[a]:
    head: a
    tail: List[a]
    def to_list(self):
        return [self.head] + self.tail.to_list()

@dataclass
class TreeNode[a]:
    l: Tree[a]
    v: a
    r: Tree[a]
    def to_list(self):
      return self.l.to_list() + [self.v] + self.r.to_list()

def flatten(ls):
    if not ls: return []
    return ls[0].to_list() + flatten(ls[1:])

ls = [ListNode(1, Empty()), TreeNode(Empty(), 2, Empty())]
ls2 = flatten(ls)
```

Fill in the type signatures of all the methods and functions and the
type annotations for the `ls` and `ls2` variables so
that the type-checker can verify that the program is type-safe. The
given type annotations should be general enough such that defining a new
class and adding an instance of it to `ls` requires no change
in type annotation:

``` python
@dataclass
class Singleton[a]:
  x: a
  def to_list(self):
    return [self.x]
    
ls = [ListNode(1, Empty()), TreeNode(Empty(), 2, Empty()),
  Singleton(3)]
# ...
```

### Question 3
Defined below is a data type describing clothing sizes.
```haskell
data Size = XS | S | M | L | XL
    deriving (Eq, Ord, Show, Bounded, Enum)
```
Proceed to define the following functions:
- `smallest` produces the smallest size
- `descending` produces a list of all the sizes from large to small
- `average` produces the average size of two sizes; in case there isn't an exact middle between two sizes, prefer the smaller one

Example runs follow.
```haskell
ghci> smallest :: Size
XS
ghci> descending :: [Size]
[XL, L, M, S, XS]
ghci> average XS L
S
```
However, take note that your functions must _not_ only work on the `Size` type. Some of these functions can be implemented with the typeclass methods that `Size` derives. You should implement your solution based on these methods so that your function can be as general as possible. In particular, we should be able to define a new type which derives these typeclasses, and all your functions should still work on them as we should expect. An example is as follows:
```haskell
ghci> :{
ghci| data Electromagnet = Radio | Micro | IR | Visible | UV | X | Gamma
ghci|    deriving (Eq, Ord, Show, Bounded, Enum)
ghci| :}
ghci> smallest :: Electromagnet
Radio
ghci> descending :: [Electromagnet]
[Gamma, X, UV, Visible, IR, Micro, Radio]
ghci> average Gamma Radio
Visible
```

### Question 4
Implement the mergesort algorithm
as a function `mergesort`. Ignoring time complexity, your
algorithm should split the list in two, recursively mergesort each half,
and merge the two sorted sublists together. Example runs follow:

``` haskell
ghci> mergesort [5,2,3,1,2]
[1,2,2,3,5]
ghci> mergesort "edcba"
"abcde"
```

### Question 5

Recall [Chapter 2.3 (Types#Algebraic Data Types)](../../types/sections/algebraic_data_types.md) where we defined an `Expr` GADT.

``` haskell
data Expr a where
  LitNumExpr :: Int -> Expr Int
  AddExpr :: Expr Int -> Expr Int -> Expr Int
  -- ...

eval :: Expr a -> a
eval (LitNumExpr x) = x
eval (AddExpr e1 e2) = eval e1 + eval e2
  -- ... 
```

Now that we have learnt typeclasses, let us attempt to *separate* each
constructor of `Expr` as *individual types*, while still
preserving functionality; the purpose of this being to keep the
`Expr` type modular and extensible:

``` haskell
data LitNumExpr = -- ...
data AddExpr = -- ...
```
while still being able to apply `eval` on any of those
expressions:

``` haskell
-- 2 + 3
ghci> eval (AddExpr (LitNumExpr 2) (LitNumExpr 3))
5
-- if 2 == 1 + 1 then 1 + 2 else 4
ghci> eval (CondExpr 
  (EqExpr (LitNumExpr 2) 
          (AddExpr (LitNumExpr 1) (LitNumExpr 1))) 
  (AddExpr (LitNumExpr 1) (LitNumExpr 2))
  (LitNumExpr 4))
3
```

Proceed to define all these different types of expressions and their
corresponding implementations for `eval`:
-   `LitNumExpr`. A literal integer, such as
    `LitNumExpr 3`.
-   `AddExpr`. An addition expression in the form of
    \\(e_1 + e_2\\), such as
    `AddExpr (LitNumExpr 1) (LitNumExpr 2)` representing
    \\(1 + 2\\)
-   `EqExpr`. An equality comparison expression in the form of
    \\(e_1 = e_2\\), such as `Eq (LitNumExpr 1) (LitNumExpr 2)`
    representing \\(1 = 2\\)
-   `CondExpr`. A conditional expression in the form of
    \\(\text{if }e\text{ then } e_1 \text{ else }e_2\\)

### Question 6
In Python, a _sequence_ is a data structure that has a _length_ and a way to obtain elements from it by integer indexing. Strings, ranges, tuples and lists are all sequences in Python:
```python-repl
>>> len([1, 2, 3])
3
>>> 'abcd'[3]
'c'
```

Our goal is to create something similar in Haskell. However, instead of loosely defining what a _sequence_ is, like Python does, we shall create a typeclass called `Sequence` and allow all types that implements these methods to become a sequence formally (at least, to the compiler)!

Proceed to define a typeclass called `Sequence` with two methods:
- `(@)` does indexing, so `ls @ i` is just like `ls[i]` in Python; if the index `i` is out of bounds, the method should panic (you can let it return `undefined` in this case)
- `len` produces the length of the sequence
- `prepend` prepends an element onto the sequence

Then define instances for `[a]` to be a sequence over `a`'s! Example runs follow:
```haskell
ghci> x :: [Int] = [1, 2, 3, 4]
ghci> x @ 2
3
ghci> x @ 4
-- some error...
ghci> len x
4
ghci> x `prepend` 5
[5, 1, 2, 3, 4]
ghci> len "abcde"
5
ghci> "abcde" @ 0
'a'
```
What's really neat about using typeclasses instead of defining a separate `Sequence` data type is that _any_ type that conforms to the specification in our `Sequence` typeclass can become a valid sequence. For example, one sequence we might want is a sequence of `()` (the unit type, which only has one constructor with no arguments, and terms of this type signify "_nothing significant_", similar to `void` in other languages).[^1] Because each element of such a sequence carries no information, instead of creating such a sequence using a list, i.e. a list of type `[()]`, we can instead use `Int` as our sequence!

```haskell
ghci> x :: Int = 4
ghci> x @ 2
()
ghci> x @ 4
-- some error...
ghci> len x
4
ghci> (x `prepend` 5) @ 4
()
```
Proceed to define a typeclass instance for `Int` such that `Int`s are sequences of `()`.

---
[^1]: This is an extremely contrived example. The main point we are driving home is that we can create very concise implementations of data structures based on domain-specific knowledge.


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
