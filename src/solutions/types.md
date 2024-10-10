![Updated][update-shield]
# Types
## Question 1

1.  `Int`.
2.  `String`. `x` has type `Int`, so `show x` has
    type `String`.
3.  `String`. Recall that `String` is an alias for
    `[Char]`. Although the expression evaluates to
    `[]` which has type `forall a. [a]`, because
    both branches of the conditional expression must have the same type,
    the type of the expression is thus specialized into
    `[Char]`.
4.  `[a] -> [a]`. `(++)` has type
    `forall a. [a] -> [a] -> [a]`, since `[]` is
    also polymorphic with type `forall a. [a]`, there is no
    need to specialize the resulting function call expression. This
    makes sense because any list can be concatenated with the empty
    list.
5.  `[Int] -> [Int]`. The `map` function has type
    `(a -> b) -> [a] -> [b]`. Since we have supplied a
    function `Int -> Int`, we are thus specializing `a` and
    `b` to `Int`.
6.  `(a -> [Int]) -> a -> String`. Recall that `(.)` has type
    `forall b c a. (b -> c) -> (a -> b) -> a -> c`. The
    function `\(x :: Int) -> show x` has type
    `Int -> String`. Thus, substituting `b` and `c` for
    `Int` and `String` respectively, we get our
    answer.
7.  `(String -> a) -> Int -> a`. Note that `(+3)` is
    `\x -> x + 3`, while `(3+)` is
    `\x -> 3 + x`. As such, the answer here follows the same
    reasoning except that the argument to `(.)` is at the second
    position.
8.  `(a, b) -> c -> (a, c)`. Note that `(,)` is the tuple
    (pair) constructor which has type
    `forall a, b. a -> b -> (a, b)`.
9.  `(a -> Bool) -> [a] -> [a]`. As we know,
    `filter` receives a function that tests each element, and
    returns the list with only the elements that pass the test.

## Question 2

1.  `eqLast`: `Eq a => [a] -> [a] -> Bool`. This function can
    be polymorphic but requires that `a` is amenable to equality
    comparisons, so we add the `Eq` constraint to it. We will discuss
    more on typeclasses next week.
2.  `isPalindrome`: `Eq a => [a] -> [a] -> Bool`. The reason
    for the `Eq` constraint is because we need to compare the two lists
    for equality, which means that the elements of both lists must be
    amenable to equality comparisons!
3.  `burgerPrice`: `Fractional a => String -> a`. Notice once
    again that we have another typeclass constraint in this function
    signature. Typeclasses are incredibly common, and hopefully this
    might motivate you to understand these in the subsequent lectures.
    Nonetheless, if you had answered `String -> Double`, that
    is fair as well.
4.  `@:`: `[a] -> (Int, Int) -> [a]`. The function receives a
    list, a pair of two integers, and produces a slice of the list of
    the same type.

## Question 3
Let us first define a type that describes
valid ingredients and a function on this type that gives their prices:

``` haskell
data Ingredient = B | C | P | V | O | M
price :: Ingredient -> Rational
price B = 0.5
price C = 0.8
price P = 1.5
price V = 0.7
price O = 0.4
price M = 0.9
```

Then, we can define a valid burger being a list of ingredients. For
this, we can define a *type alias* like so:

``` haskell
type Burger = [Ingredient]
```

Type aliases are nothing special; more or less, they are *nicknames* for
types. There is no difference between the `Burger` and
`[Ingredient]` types, just like how there is no difference
between `String` and `[Char]`. Then, we can define
our `burgerPrice` function with pattern matching in a very standard way:

``` haskell
burgerPrice :: Burger -> Rational
burgerPrice [] = 0
burgerPrice (i : is) = price i + burgerPrice is
```

Let us take this a step further by observing the following function in
Haskell's prelude:

``` haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f n [] = n
foldr f n (x : xs) = 
  let r = foldr f n xs
  in  f x r
```

In practice, this does something very familiar:
\\[\textit{foldr}(f, n, [a_1,\dots,a_n]) = f(a_1,f(a_2,\dots f(a_{n-1}, f(a_n, n))\dots ))\\]

This looks like the right-associative equivalent of `reduce` in Python!
(The equivalent of `reduce` in Haskell is the `foldl` function).
\\[\textit{reduce}(f, n, [a_1,\dots,a_n]) = f(f(\dots f(n, a_1), a_2), \dots, a_n)\\]
This hints to us that in the definition of `foldr`, `f` is the combiner
function and `n` is the initial value. This corresponds very nicely to
`burgerPrice`. Let us try rewriting our `burgerPrice` function to see
this:

``` haskell
burgerPrice [] = 0
burgerPrice (x : xs) = 
  let r = burgerPrice xs
      f a b = price a + b
      -- alternatively, 
      -- f = (+) . price
  in  f x r
```

As you can see, if we let `f` be `(+) . price` and `n` be `0`, we can
define `burgerPrice` based on `foldr`:

``` haskell
burgerPrice = foldr ((+) . price) 0
```

## Question 4
Solutions are self-explanatory.

``` haskell
dropConsecutiveDuplicates :: Eq a => [a] -> [a]
dropConsecutiveDuplicates [] = []
dropConsecutiveDuplicates [x] = [x]
dropConsecutiveDuplicates (x : xx : xs) 
  | x == xx   = dropConsecutiveDuplicates (x : xs)
  | otherwise = x : dropConsecutiveDuplicates (xx : xs)
```

## Question 5
As hinted by the example runs, a zipper is a tuple of two
lists. The idea is to model a zipper as two stacks. This is great
because singly-linked lists (with head pointers), as we know, can model
stacks.

``` haskell
type ListZipper a = ([a], [a])
mkZipper :: [a] -> ListZipper a
mkZipper ls = ([], ls)
```

Functions for traversing and replacing the elements of the zipper should
be straightforward to define. Note that the `@` symbol binds the entire
pattern on the right to the name on the left.

``` haskell
l, r :: ListZipper a -> ListZipper a

l x@([], _) = x
l (x : xs, ys) = (xs, x : ys)

r x@(_,[]) = x
r (xs, y : ys) = (y : xs, ys)

setElement :: a -> ListZipper a -> ListZipper a
setElement x (xs,[]) = (xs, [x])
setElement x (xs, _ : ys) = (xs, x : ys)
```

## Question 6
To start, we define a binary tree. This is very similar to the
tree examples that we have given, except that we allow the tree to be
empty. Note that you might be tempted to put the `Ord`
constraint at the data type declaration itself. This is deprecated, and
also not recommended.

``` haskell
data SortedSet a = Empty | Node (SortedSet a) a (SortedSet a)
```

Let us start with the function to add elements to the sorted set. This
should be straightforward if you remember how BST algorithms are
defined.

``` haskell
(@+) :: Ord a => SortedSet a -> a -> SortedSet a
Empty @+ x = Node Empty x Empty
t@(Node left a right) @+ x
    | x == a     = t
    | x < a      = Node (left @+ x) a right
    | otherwise  = Node left a (right @+ x)
```

Given a BST, to get the list of elements in sorted order, perform an
inorder traversal.

``` haskell
setToList :: SortedSet a -> [a]
setToList Empty = []
setToList (Node left a right) = setToList left ++ (a : setToList right)
```

Converting a list into a sorted set can be done by repeated applications
of `@+` over the elements of the list. This should hint to us that we
can use a fold over the list. Note that the `flip` function flips the
arguments of a function: i.e. `flip f x y = f y x`.

``` haskell
sortedSet :: Ord a => [a] -> SortedSet a 
sortedSet = foldr (flip (@+)) Empty
```

Finally, determining if an element is a member of the sorted set is a
matter of binary search.

``` haskell
in' :: Ord a => a -> SortedSet a -> Bool
in' _ Empty = False
in' x (Node left a right)
  | x == a    = True
  | x < a     = in' x left
  | otherwise = in' x right
```

An alternative to this implementation is to use AVL trees instead of
plain BSTs. We provide an implementation of AVL trees at the end of this chapter.

## Question 7
We start with the base definition which should
be self-explanatory.

``` haskell
-- Haskell
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle w h) = w * h
```

``` python
from abc import ABC, abstractmethod
from dataclasses import dataclass
from math import pi

class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass

@dataclass
class Circle(Shape):
    radius: float
    def area(self) -> float:
        return pi * self.radius ** 2

@dataclass
class Rectangle(Shape):
    width: float
    height: float
    def area(self) -> float:
        return self.width * self.height
```

We start with the first extension of our problem by creating a new shape
called `Triangle`. Notice that to add representations of our types in
our Haskell implementation, we must have access to edit whatever we've
written before. This is unlike our OO implementation in Python, where by
adding a new shape, we can just define a completely separate subclass
and define the `area` method for that class.

``` haskell
data Shape = Circle Double 
           | Rectangle Double Double
           | Triangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle w h) = w * h
area (Triangle w h) = w * h / 2
```

``` python
@dataclass
class Triangle(Shape):
    width: float
    height: float
    def area(self) -> float:
        return self.width * self.height / 2
```

However, proceeding with the second extension, we see that the opposite
is true: adding a new function does not require edit access in our
Haskell implementation since we can just define a separate function, but
it is required for our Python implementation since we have to add this
method to all the classes we have defined!

``` haskell
scale :: Double -> Shape -> Shape
scale n (Circle r) = Circle (r * n)
scale n (Rectangle w h) = Rectangle (w * n) (h * n)
scale n (Triangle w h) = Triangle (w * n) (h * n)
```

``` python
class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass
    @abstractmethod
    def scale(self, n: float) -> 'Shape':
        pass

@dataclass
class Circle(Shape):
    radius: float
    def area(self) -> float:
        return pi * self.radius ** 2
    def scale(self, n: float) -> Shape:
        return Circle(n * self.radius)

@dataclass
class Rectangle(Shape):
    width: float
    height: float
    def area(self) -> float:
        return self.width * self.height
    def scale(self, n: float) -> Shape:
        return Rectangle(self.width * n, self.height * n)

@dataclass
class Triangle(Shape):
    width: float
    height: float
    def area(self) -> float:
        return self.width * self.height / 2
    def scale(self, n: float) -> Shape:
        return Triangle(self.width * n, self.height * n)
```

## Question 8
Defining additional constructors for our expressions GADT is
relatively straightforward, and so is extending our `eval`
function. We write the entire implementation here.

``` haskell
{-# LANGUAGE GADTs #-}
data Expr α where
  LitNumExpr  :: Int -> Expr Int
  AddExpr     :: Expr Int -> Expr Int -> Expr Int
  EqExpr      :: Eq α => Expr α -> Expr α -> Expr Bool
  CondExpr    :: Expr Bool -> Expr α -> Expr α -> Expr α
  LitBoolExpr :: Bool -> Expr Bool
  AndExpr     :: Expr Bool -> Expr Bool -> Expr Bool
  OrExpr      :: Expr Bool -> Expr Bool -> Expr Bool
  FuncExpr    :: (α -> β) -> Expr (α -> β)
  FuncCall    :: Expr (α -> β) -> Expr α -> Expr β

eval :: Expr α -> α
eval (LitNumExpr n)   = n
eval (AddExpr a b)    = eval a + eval b
eval (EqExpr a b)     = eval a == eval b
eval (CondExpr a b c) = if eval a then eval b else eval c
eval (LitBoolExpr b)  = b
eval (AndExpr a b)    = eval a && eval b
eval (OrExpr a b)     = eval a || eval b
eval (FuncExpr f)     = f
eval (FuncCall f x)   = (eval f) (eval x)
```

## Question 9
### Bank Accounts
#### Bank Account ADT
As in the lecture notes, simulating ADTs in Python can be done either
with an (abstract) class, or a type alias. In our case, we shall use the
latter.

First, we create the type:

``` python
type BankAccount = NormalAccount | MinimalAccount
```

Then, we create the `NormalAccount` and
`MinimalAccount` classes:

``` python
from dataclasses import dataclass

@dataclass(frozen=True)
class NormalAccount:
    account_id: str
    balance: float
    interest_rate: float

@dataclass(frozen=True)
class MinimalAccount:
    account_id: str
    balance: float
    interest_rate: float
```

#### Basic Features
For our two basic features, we shall employ a simple helper function
that sets the amount of a bank account. Notice once again that we do not
mutate any data structure in our program!

``` python
def _set_balance(amt: float, b: BankAccount) -> BankAccount:
    match b:
        case NormalAccount(id, _, i):
            return NormalAccount(id, amt, i)
        case MinimalAccount(id, _, i):
            return MinimalAccount(id, amt, i)
```

Then, the basic features can be defined in terms of our
`_set_balance` helper function.

``` python
def deposit(amt: float, b: BankAccount) -> BankAccount:
    return _set_balance(b.balance + amt, b)

def deduct(amt: float, b: BankAccount) -> tuple[bool, BankAccount]:
    if amt > b.balance:
        return (False, b)
    return (True, _set_balance(b.balance - amt, b))
```

#### Advanced Features

At this point, implementing the advanced features should not be too
difficult.

``` python
def _cmpd(p: float, r: float) -> float:
    return p * (1 + r)

def compound(b: BankAccount) -> BankAccount:
    match b:
        case NormalAccount(id, bal, i):
            return NormalAccount(id, _cmpd(bal, i), i)
        case MinimalAccount(id, bal, i):
            new_bal: float = max(bal - 20, 0) if bal < 1000 else bal
            return MinimalAccount(id, _cmpd(new_bal, i), i)

def transfer(amt: float, from_: BankAccount, to: BankAccount) -> tuple[bool, BankAccount, BankAccount]:
    success: bool
    from_deducted: BankAccount
    success, from_deducted = deduct(amt, from_)
    if not success:
        return (False, from_, to)
    return (True, from_deducted, deposit(amt, to))
```

### Operating on Bank Accounts
#### Operations ADT
The ADT definition is pretty straightforward:

``` python
type Op = Transfer | Compound

@dataclass
class Transfer:
    amount: float
    from_: str
    to: str

@dataclass
class Compound:
    pass 
```

#### Processing One Operation

It's easier to write the functions that perform each individual
operation first, especially since they are more involved with dictionary
lookups etc. Take note of the fact that all of the data structures are
unchanged!

``` python
# Type alias for convenience
type BankAccounts = dict[str, BankAccount]

def _compound_all(mp: BankAccounts) -> BankAccounts:
    return {k : compound(v) for k, v in mp.items()}

def _transfer(amt: float, from_: str, to: str, mp: BankAccounts) -> tuple[bool, BankAccounts]:
    if from_ not in mp or to not in mp:
        return (False, mp)
    success: bool
    new_from: BankAccount
    new_to: BankAccount
    success, new_from, new_to = transfer(amt, mp[from_], mp[to])
    if not success:
        return (False, mp)
    new_mp: BankAccounts = mp | { from_: new_from, to: new_to }
    return (True, new_mp)
```

Then, the `process_one` function is easy to define since we can
just invoke our helper functions:

``` python
def process_one(op: Op, mp: BankAccounts) -> tuple[bool, BankAccounts]:
    match op:
        case Transfer(amt, from_, to):
            return _transfer(amt, from_, to, mp)
        case Compound():
            return (True, _compound_all(mp))
```

#### Process All Operations
Given the `process_one` function, the `process_all`
function should be straightforward. Note once again that none of the
data structures are being mutated and we use recursion. The last
`case` statement is only used to suppress `pyright` warnings.

``` python
def process_all(ops: list[Op], mp: BankAccounts) -> tuple[list[bool], BankAccounts]:
    match ops:
        case []:
            return [], mp
        case x, *xs:
            op_r, mp1 = process_one(x, mp)
            rs, mp2 = process_all(xs, mp1)
            return [op_r] + rs, mp2
        case _: raise
```

#### Polymorphic Processing
Notice that if we had received the `process_one` function as an
argument then we would now have a higher-order function:

``` python
from typing import Callable
# For brevity
type P = Callable[[Op, BankAccounts], tuple[bool, BankAccounts]]
def process_all(process_one: P, ops: list[Op], mp: BankAccounts) -> tuple[list[bool], BankAccounts]:
    match ops:
        case []:
            return [], mp
        case x, *xs:
            op_r, mp1 = process_one(x, mp)
            rs, mp2 = process_all(process_one, xs, mp1)
            return [op_r] + rs, mp2
        case _: raise
```

Now notice that `process_all`'s implementation does not depend
on `Op`, `bool` or `BankAccounts`. Let us
make this function polymorphic by replacing `Op` with `A`,
`BankAccounts` with `B` and `bool` with `C`!

``` python
def process[A, B, C](f: Callable[[A, B], tuple[C, B]], ops: list[A], mp: B) -> tuple[list[C], B]:
    match ops:
        case []:
            return [], mp
        case x, *xs:
            op_r, mp1 = f(x, mp)
            rs, mp2 = process(f, xs, mp1)
            return [op_r] + rs, mp2
        case _: raise
```

## AVL Trees

Here we show an example of using AVL trees as sorted sets. Notice our
AVL tree has nice pretty printing, pretty cool huh! We will learn how to
define the string representation of a type in subsequent lectures.

    ghci> x = fromList [1,1,1,2,2,2,8,5,4,3,5,9,0,10,0,7,8,3]
    ghci> x
                7
          ┏━━━━━┻━━━┓
          3         9
      ┏━━━┻━━━┓   ┏━┻━┓
      1       5   8   10
    ┏━┻━┓   ┏━┛
    0   2   4
    ghci> x @+ 6 @+ 11 @+ 14 @+ 12 @+ 15
                  7
          ┏━━━━━━━┻━━━━━━━━┓
          3                11
      ┏━━━┻━━━┓       ┏━━━━┻━━━━━┓
      1       5       9          14
    ┏━┻━┓   ┏━┻━┓   ┏━┻━┓     ┏━━┻━━┓
    0   2   4   6   8   10    12    15

We first start with some declarations and imports.

``` haskell
module Avl ( AVL(Empty), in', toList, fromList, (@+)) where

import Data.List (intercalate)

data AVL a = Empty | Node (AVL a) a (AVL a) 
  deriving Eq

in'      :: Ord a => a -> AVL a -> Bool
toList   :: AVL a -> [a]
fromList :: Ord a => [a] -> AVL a
(@+)     :: Ord a => AVL a -> a -> AVL a
infixl 7 @+
```

Next, we provide implementations of these declarations. Many of these
are identical to that of our sorted set implementation using BSTs; the
only difference is in `@+` where AVL trees have to perform height
balancing if the balance factor exceeds the range \\([-1, 1]\\).

``` haskell
in' _ Empty = False
in' x (Node left a right)
  | x == a    = True
  | x < a     = in' x left
  | otherwise = in' x right

toList Empty = []
toList (Node left a right) = toList left ++ (a : toList right)

fromList = foldr (flip (@+)) Empty

Empty @+ x = Node Empty x Empty
o@(Node left a right) @+ x 
  | x < a = 
      let newLeft = left @+ x
          newTree = Node newLeft a right
      in  if bf newTree > -2 then newTree
          else 
            let t 
                  | bf newLeft > 0 = Node (rotateLeft newLeft) a right 
                  | otherwise      = newTree
            in rotateRight t
  | x > a =
      let newRight = right @+ x
          newTree = Node left a newRight
      in  if bf newTree < 2 then newTree
          else let t
                    | bf newRight < 0 = Node left a (rotateRight newRight)
                    | otherwise       = newTree
               in rotateLeft t
  | otherwise = o
```

The implementation of these functions involve some additional helper
functions for obtaining balance factors and rotations, which we declare
and define here:

``` haskell
-- Implementation helpers
height :: AVL a -> Int
height Empty = 0
height (Node left _ right) = 1 + max (height left) (height right)

rotateLeft :: AVL a -> AVL a
rotateLeft Empty = Empty
rotateLeft t@(Node _ _ Empty) = t
rotateLeft (Node left a (Node ll b right)) = Node (Node left a ll) b right

rotateRight :: AVL a -> AVL a
rotateRight Empty = Empty
rotateRight t@(Node Empty _ _) = t
rotateRight (Node (Node left b rr) a right) = Node left b (Node rr a right)

bf :: AVL a -> Int -- balance factor
bf Empty = 0
bf (Node l _ r) = height r - height l
```

Finally, we write functions to support pretty printing.

``` haskell
-- Pretty printing
strWidth :: Show a => AVL a -> Int
strWidth Empty = 0
strWidth (Node left a right) = 
  let leftWidth = strWidth left
      l = if leftWidth > 0 then leftWidth + 1 else 0
      centerWidth = length $ show a
      rightWidth = strWidth right
      r = if rightWidth > 0 then rightWidth + 1 else 0
  in  l + centerWidth + r

leftPad :: Int -> String -> String
leftPad 0 s = s
leftPad n s = leftPad (n - 1) (' ' : s)

rightArm, leftArm :: Int -> String

rightArm n = aux n where
  aux n' 
    | n' == n   = '┗' : aux (n' - 1)
    | n' > 0    = '━' : aux (n' - 1)
    | otherwise = "┓"

leftArm n = aux n where
  aux n'
    | n' == n = '┏' : aux (n' - 1)
    | n' > 0  = '━' : aux (n' - 1)
    | otherwise = "┛"

bothArm :: Int -> Int -> String
bothArm mid right = aux 0 where
  aux n'
    | n' == 0 = '┏' : aux 1
    | n' /= mid && n' < right = '━' : aux (n' + 1)
    | n' == mid = '┻' : aux (n' + 1)
    | otherwise = "┓"

toRowList :: Show a => AVL a -> [String]
toRowList Empty = []
toRowList (Node Empty a Empty) = [show a]
toRowList (Node Empty a right) =
  let x = toRowList right
      nodeLength = length $ show a
      y = map (leftPad (nodeLength + 1)) x
      rroot = rootAt right + nodeLength + 1
  in show a : rightArm rroot : y
toRowList (Node left a Empty) = 
  let x = toRowList left
      lroot = rootAt left
      nodeAt = strWidth left + 1
  in leftPad nodeAt (show a) : leftPad lroot (leftArm (nodeAt - lroot)) : x
toRowList (Node left a right) = 
  let l = toRowList left
      r = toRowList right
      lw = strWidth left
      rpadding = lw + 2 + length (show a)
      rr = zipStringTree rpadding l r
      lroot = rootAt left
      rroot = rootAt right
      nodeAt = lw + 1
      f = leftPad (lw + 1) (show a)
      s = leftPad lroot (bothArm (nodeAt - lroot) (rroot - lroot + rpadding))
  in  f : s : rr


rightPadTo :: Int -> String -> String
rightPadTo n s
  | ls >= n   = s
  | otherwise = let n' = n - ls
                    s' = leftPad n' []
                in  s ++ s'
  where ls = length s

rootAt :: Show a => AVL a -> Int
rootAt Empty = 0
rootAt (Node Empty _ _) = 0
rootAt (Node left _ _) = strWidth left + 1

zipStringTree :: Int -> [String] -> [String] -> [String]
zipStringTree _ [] [] = []
zipStringTree _ l [] = l
zipStringTree n [] r = map (leftPad n) r
zipStringTree n (l : ls) (r : rs) = 
  let res = zipStringTree n ls rs
      c   = rightPadTo n l ++ r
  in  c : res

instance Show a => Show (AVL a) where
  show Empty = ""
  show t = intercalate "\n" $ toRowList t
```


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
