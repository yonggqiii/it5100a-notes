![Updated][update-shield]

### Question 1
To implement these classes and methods, just "convert" the Haskell definitions to Python code. Note that `Validation` is _not_ a monad.

```python
from typing import Any
from dataclasses import dataclass

class List:
    @staticmethod
    def pure(x): return Node(x, Empty())

    # Convenience method for Question 3
    @staticmethod
    def from_list(ls):
        match ls:
            case []: return Empty()
            case x, *xs: return Node(x, List.from_list(xs))

@dataclass
class Node(List):
    head: object
    tail: List

    def map(self, f):
        return Node(f(self.head), self.tail.map(f))

    def ap(self, x):
        tails = self.tail.ap(x)
        heads = Node._ap(self.head, x) 
        return heads.concat(tails)

    # helper method
    @staticmethod
    def _ap(f, xs):
        match xs:
            case Empty(): return Empty()
            case Node(l, r): return Node(f(l), Node._ap(f, r))

    def concat(self, xs):
        return Node(self.head, self.tail.concat(xs))

    def flatMap(self, f):
        return f(self.head).concat(self.tail.flatMap(f))

@dataclass
class Empty(List):
    def map(self, f): return self
    def concat(self, xs): return xs
    def ap(self, x): return self
    def flatMap(self, f): return self

class Maybe:
    @staticmethod
    def pure(x): return Just(x)

@dataclass
class Just(Maybe):
    val: object

    def map(self, f): return Just(f(self.val))

    def ap(self, x): 
        match x:
            case Just(y): return Just(self.val(y))
            case Nothing(): return x

    def flatMap(self, f): return f(self.val)

@dataclass
class Nothing:
    def map(self, f): return self
    def ap(self, x): return self
    def flatMap(self, f): return self

class Either:
    @staticmethod
    def pure(x): return Right(x)

@dataclass
class Left(Either):
    inl: object
    def map(self, f): return self
    def ap(self, f): return self
    def flatMap(self, f): return self

@dataclass
class Right(Either):
    inr: object

    def map(self, f): return Right(f(self.inr))

    def ap(self, f): 
        match f:
            case Left(e): return f
            case Right(x): return Right(self.inr(x))

    def flatMap(self, f): return f(self.inr)

class Validation:
    @staticmethod
    def pure(x): return Success(x)

@dataclass
class Success:
    val: object

    def map(self, f): return Success(f(self.val))

    def ap(self, f): 
        match f:
            case Failure(e): return f
            case Success(x): return Success(self.val(x))


@dataclass
class Failure:
    err: list[str]

    def map(self, f): return self

    def ap(self, f):
        match f:
            case Failure(err): return Failure(self.err + err)
            case Success(x): return self
```

### Question 2

#### Question 2.1: Unsafe Sum
The Python implementation of `sum_digits` can be a Haskell rewrite of your `sumDigits` solution for Question 6 in [Chapter 1.4 (Course Introduction#Exercises)](../course_introduction/sections/exercises.md):

```python
def sum_digits(n):
    return n if n < 10 else \
           n % 10 + sum_digits(n // 10)
```

#### Question 2.2: Safe Sum
The idea is to have `sum_digits` return a `Maybe` object. In particular, the function should return `Nothing` if `n` is negative, and `Just x` when `n` is positive and produces result `x`.

```python
def sum_digits(n):
    return Nothing() if n < 0 else \
           Just(n)   if n < 10 else \
           sum_digits(n // 10).map(lambda x: x + n % 10)
```

```haskell
sumDigits :: Int -> Maybe Int
sumDigits n
  | n < 0 = Nothing
  | n < 10 = Just n
  | otherwise = (n `mod` 10 +) <$> sumDigits (n `div` 10)
```

#### Question 2.3: Final Sum
The result of `sum_digits` is a `Maybe[int]`, and `sum_digits` itself has type `int -> Maybe[int]`. To compose `sum_digits` with itself we can use `flatMap` or `>>=`.

```python
def final_sum(n):
    n = sum_digits(n)
    return n.flatMap(lambda n2: n2 if n2 < 10 else final_sum(n2))
```

```haskell
finalSum :: Int -> Maybe Int
finalSum n = do
  n' <- sumDigits n
  if n' < 10 
  then Just n'
  else finalSum n'
```

### Question 3
#### Question 3.1: Splitting Strings
`split` in Python can be implemented with the `str.split` method. The `split` function for Haskell is shown in [Chapter 4.4 (Railway Pattern#Validation)](../railway_pattern/validation.md).

```python
# Uses the convenience method from_list in the List class
def split(char, s):
    return List.from_list(s.split(char))
```
#### Question 3.2: CSV Parsing
Split the string over `\n`, then split each string in that list over `,` using `map`:

```python
def csv(s):
    return split('\n', s)
                .map(lambda x: split(',', x))
```

```haskell
csv :: String -> [[String]]
csv s = split ',' <$> (split '\n' s)
```

### Question 4
#### Question 4.1: Factorial
Should be boring at this point.
```python
def factorial(n):
    return 1 if n <= 1 else \
           n * factorial(n - 1)
```

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

#### Question 4.2: Safe Factorial
The idea is to return a `Left` if `n` is negative, `Right` with the desired result otherwise. Typically, `Right` is the happy path.

```python
def factorial(n, name):
    if n < 0: 
        return Left(name + ' cannot be negative!')
    if n <= 1:
        return Right(1)
    return factorial(n - 1, name).map(lambda x: x * n)
```

```haskell
factorial :: Int -> String -> Either String Int
factorial n name
  | n < 0 = Left $ name ++ " cannot be negative!"
  | n <= 1 = Right 1
  | otherwise = (n*) <$> factorial (n - 1) name
```

#### Question 4.3: Safe n choose k
Idea: Compute \\(n!\\), \\(k!\\) and \\((n - k)!\\) in "parallel", combine with `ap`:

```python
def choose(n, k):
    nf = factorial(n, 'n')
    kf = factorial(k, 'k')
    nmkf = factorial(n - k, 'n - k')
    div = lambda x: lambda y: lambda z: x // y // z
    return nf.map(div).ap(kf).ap(nmkf)    
```

```haskell
choose :: Int -> Int -> Either String Int
choose n k 
    = let nf   = factorial n "n"
          kf   = factorial k "k"
          nmkf = factorial (n - k) "n - k"
          f x y z = x `div` y `div` z
      in f <$> nf <*> kf <*> nmkf
```

With the `ApplicativeDo` language extension enabled, you can just use `do` notation:

```haskell
{-# LANGUAGE ApplicativeDo #-}
choose :: Int -> Int -> Either String Int
choose n k = do
  nf <- factorial n "n"
  kf <- factorial k "k"
  nmkf <- factorial (n - k) "n - k"
  return $ nf `div` kf `div` nmkf 
```


#### Question 4.4
Redefine `factorial` to use `Validation` instead of `Either`:

```python
def factorial(n, name):
    if n < 0:
        return Failure([f'{name} cannot be negative!'])
    if n <= 1:
        return Success(1)
    else:
        return factorial(n - 1, name).map(lambda x: n * x)

```
```haskell
factorial :: Int -> String -> Validation [String] Int
factorial n name
  | n < 0 = Failure [name ++ " cannot be negative!"]
  | n <= 1 = Success 1
  | otherwise = (n*) <$> factorial (n - 1) name
```
Finally, update the type signature of `choose` (we do not need to do so in Python).

```haskell
choose :: Int -> Int -> Validation [String] Int
choose n k = do
  nf <- factorial n "n"
  kf <- factorial k "k"
  nmkf <- factorial (n - k) "n - k"
  return $ nf `div` kf `div` nmkf 
```

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
