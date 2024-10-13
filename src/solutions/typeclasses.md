![Updated][update-shield]

# Typeclasses

### Question 1
1.  `Num a => a`. Because all of `1`, `2` and `3` can be interpreted as any number, the entire expression can likewise be interpreted as any number.
2.  `Show b => (a -> b) -> a -> String`. The type of `show` is `Show a => a -> String`, in other words, any type that implements the `Show` typeclass can be converted into a `String`. Therefore, `(show .)` can receive any function `a -> b` where `b` implements `Show`, so that the result is a function that receives `a` and produces `String`
3.  `Show a => (String -> b) -> a -> b`. Similar to the above.
4. `Eq a => (a, a) -> Bool`. The elements of the tuple must be amenable to equality comparisons, and therefore must be of the same type `a` where `a` implements `Eq`. 

### Question 2
The idea is to create a protocol that describes classes that
have a `to_list` function. In the following solution, the
protocol is called `ToList`.

``` python
from typing import Any

type Tree[a] = Empty | TreeNode[a]
type List[a] = Empty | ListNode[a]

@dataclass
class Empty:
    def to_list(self) -> list[Any]:
        return []

@dataclass
class ListNode[a]:
    head: a
    tail: List[a]
    def to_list(self) -> list[a]:
        return [self.head] + self.tail.to_list()

@dataclass
class TreeNode[a]:
    l: Tree[a]
    v: a
    r: Tree[a]
    def to_list(self) -> list[a]:
        return self.l.to_list() + [self.v] + self.r.to_list()

class ToList[a](Protocol):
    def to_list(self) -> list[a]:
        raise

def flatten[a](ls: list[ToList[a]]) -> list[a]:
    if not ls: return []
    return ls[0].to_list() + flatten(ls[1:])

ls: list[ToList[int]] = [ListNode(1, Empty()), TreeNode(Empty(), 2, Empty())]
ls2: list[int] = flatten(ls)
```

### Question 3
The `smallest` function can be implemented directly with the `minBound` method of the `Bounded` typeclass:
```haskell
smallest :: Bounded a => a 
smallest = minBound
```
The `descending` function can also be implemented directly with the `Bounded` and `Enum` methods. The idea is to construct a range (which requires `Enum`) starting from `maxBound` and enumerating all the way to `minBound`. You can either construct a range starting from `minBound` to `maxBound` and then reverse the list, or you can start from `maxBound`, followed by `pred maxBound` (`pred` comes from `Enum`), and end at `minBound`.

```haskell
descending :: (Bounded a, Enum a) => [a]
descending = [maxBound,pred maxBound..minBound]
```

The `average` function can be implemented by converting the two terms to integers using `fromEnum`, then take the average, and use `toEnum` to bring it back to the desired term.

```haskell
average :: Enum a => a -> a -> a
average x y = toEnum $ (fromEnum x + fromEnum y) `div` 2
```
 

### Question 4
Any list of elements that can be ordered, i.e. any list over a type implementing `Ord` can be sorted!

``` haskell
import Data.List (splitAt)
mergesort :: Ord a => [a] -> [a]
mergesort ls 
  | len <= 1 = ls
  | otherwise = let (l, r) = splitAt (len `div` 2) ls
                    l'     = mergesort l
                    r'     = mergesort r
                in  merge l' r'
  where len :: Int
        len = length ls
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] x = x
        merge x [] = x
        merge l@(x : xs) r@(y : ys)
          | x <= y = x : merge xs r
          | otherwise = y : merge l ys
```

### Question 5
Before we even begin, it will be helpful to decide what
our typeclass will look like. The typeclass should be abstracted over
the type of expression and the type from evaluating it. Therefore, it
should be something like `Expr e a`, where
`eval :: e -> a`. However, we know that `e` uniquely
characterizes `a`, therefore we should add this as a
functional dependency of our typeclass.

``` haskell
class Expr e a | e -> a
  eval :: e -> a

-- for clarity
type IntExpr e = Expr e Int
type BoolExpr e = Expr e Bool
```

Then, our types will all contain types that implement the
`Expr` typeclass.

First, to start we have numeric literals, which is straightforward.

``` haskell
data LitNumExpr = LitNumExpr Int

instance Expr LitNumExpr Int where
  eval :: LitNumExpr -> Int
  eval (LitNumExpr x) = x
```

`AddExpr` is more interesting. We require that the component
expressions must be evaluated to an `Int`. As such, we
constrain the component addends with `IntExpr` as follows:

``` haskell
data AddExpr where
  AddExpr :: (IntExpr e, IntExpr e') => e -> e' -> AddExpr

instance Expr AddExpr Int where
  eval :: AddExpr -> Int
  eval (AddExpr e1 e2) = eval e1 + eval e2
```

To define `EqExpr`, we have to allow expressions of any type
that evaluates to any type that is amenable to equality comparisons:

``` haskell
data EqExpr where
  EqExpr :: (Eq a, Expr e a, Expr e' a) => e -> e' -> EqExpr

instance Expr EqExpr Bool where
  eval :: EqExpr -> Bool
  eval (EqExpr e1 e2) = eval e1 == eval e2
```

Finally, to define a `CondExpr` we must allow it to evaluate
to any type, and thus should be parameterized.

``` haskell
data CondExpr a where
  CondExpr :: (BoolExpr c, Expr e a, Expr e' a) 
           => c -> e -> e' -> CondExpr a

instance Expr (CondExpr a) a where
  eval :: CondExpr a -> a
  eval (CondExpr c e1 e2) = if eval c then eval e1 else eval e2
```
### Question 6
As per usual, we are going to define a typeclass `Sequence` that defines the methods `@`, `len` and `prepend`. The type parameters of `Sequence` is tricky. One possibility is for `Sequence` to be higher-kinded:

```haskell
class Sequence e s where
    (@) :: s e -> Int -> e
    len :: s e -> Int
    prepend :: s e -> e -> s e

instance Sequence [] a where
    -- ...
```
However, this will not work when having `Int`s as sequences because `Int` is not a type constructor. Therefore, we will just let `s` be the full sequence type, and introduce a functional dependency `s -> e` so that the sequence type `s` _uniquely characterizes_ the type of the elements of that sequence:
```haskell
class Sequence e s | s -> e where
    (@) :: s -> Int -> e
    len :: s -> Int
    prepend :: s -> e -> s
```

In which case, the `Sequence` instances for `[a]` and `Int` becomes quite straightforward:

```haskell
instance Sequence a [a] where
  (@) :: [a] -> Int -> a
  (@) = (!!)

  len :: [a] -> Int
  len = length

  prepend :: [a] -> a -> [a]
  prepend = flip (:)

instance Sequence () Int where
  (@) :: Int -> Int -> ()
  i @ j 
    | j < 0 || j >= i = undefined
    | otherwise       = ()

  len :: Int -> Int
  len = id

  prepend :: Int -> () -> Int
  prepend = const . (+1)
```

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-13%20OCT%202024-57ffd8?style=for-the-badge
