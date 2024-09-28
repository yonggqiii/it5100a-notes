![Updated][update-shield]

# Exercises

## Question 1
Without using GHCI, determine the types of the following expressions:

1.  `(1 :: Int) + 2 * 3`
2.  `let x = 2 + 3 in show x`
3.  `if "ab" == "abc" then "a" else []`
4.  `(++ [])`
5.  `map (\(x :: Int) -> x * 2)`
6.  `((\(x :: [Int]) -> show x) . )`
7.  `( . (\(x :: [Int]) -> show x))`
8.  `(,) . fst`
9.  `filter`

## Question 2
Without the help of GHCI, describe the types of `eqLast`, `isPalindrome`, `burgerPrice` and `(@:)` which we defined in [Chapter 1.4 (Course Introduction#Exercises)](../../course_introduction/sections/exercises.md)

## Question 3
Recall the following definition of `burgerPrice`:

``` haskell
burgerPrice burger 
  | null burger = 0
  | otherwise   =
      let first = ingredientPrice (head burger)
          rest  = burgerPrice (tail burger)
      in  first + rest
  where ingredientPrice i
          | i == 'B' = 0.5
          | i == 'C' = 0.8
          | i == 'P' = 1.5
          | i == 'V' = 0.7
          | i == 'O' = 0.4
          | i == 'M' = 0.9
```

There are several problems with this. First of all, writing
`burgerPrice` with guards does not allow us to rely on
compiler exhaustiveness checks, and may give us some additional warnings
about `head` and `tail` being *partial*, despite their use being
perfectly fine. The second problem is that we have allowed our burger to
be any string, even though we should only allow strings that are
composed of valid ingredients&mdash;the compiler will not reject invocations of
`burgerPrice` with bogus arguments like `"AbcDEF"`.

Define a new type that represents valid burgers, and re-define
`burgerPrice` against that type using pattern matching.
Additionally, provide a type declaration for this function. Note that
you may use the `Rational` type to describe rational numbers
like `0.8` etc, instead of `Double` which may have
precision issues. You might see that the output of your
`burgerPrice` function is of the form `x % y` which means
\\(x/y\\).

## Question 4
Define a function `dropConsecutiveDuplicates` that receives a
list of any type that is amenable to equality comparisons and removes
all the consecutive duplicates of the list. Example runs follow:

``` haskell
ghci> dropConsecutiveDuplicates []
[]
ghci> dropConsecutiveDuplicates [1, 2, 2, 3, 3, 3, 3, 4, 4]
[1, 2, 3, 4]
ghci> dropConsecutiveDuplicates "aabcccddeee"
"abcde"
```

For this function to be polymorphic, you will need to add a constraint
`Eq a =>` at the beginning of the function's type signature
just like we did for the `EqExpr` constructor of our
`Expr a` GADT.

## Question 5
Suppose we have a list `[1,2,3,4,5]`. Since lists in Haskell are singly-linked lists,
and not to mention that Haskell lists are immutable, changing the values
at the tail end of the list (e.g. `4` or `5`) can be inefficient! Not
only that, if we want to then change something near the element we've
just changed, we have to traverse all the way down to that element from
the head all over again!

Instead, what we can use is a *zipper*, which allows us to focus on a
part of a data structure so that accessing those elements and walking
around it is efficient. The idea is to write functions that let us walk
down the list, do our changes, and walk back up to recover the full
list. For this, we shall define some functions:

1.  `mkZipper` which receives a list and makes a zipper
2.  `r` which walks to the right of the list zipper
3.  `l` which walks to the left of the list zipper
4.  `setElement x` which changes the element at the current position of
    the zipper to `x`.

Example runs follow:
``` haskell
ghci> x = mkZipper [1,2,3,4,5]
ghci> x
([], [1,2,3,4,5])
ghci> y = r $ r $ r $ r x
ghci> y = ([4,3,2,1], [5])
ghci> z = setElement (-1) y
ghci> z
([4,3,2,1], [-1])
ghci> w = setElement (-2) $ l z
ghci> w 
([3,2,1], [-2,-1])
ghci> l $ l $ l w
([], [1,2,3,-2,-1])
```

## Question 6
Let us create a data structure that represents
sorted sets. These are collections that contain unique elements and are
sorted in ascending order. A natural data structure that can represent
such sets is the Binary Search Tree (BST) abstract data type (ADT).

Create a new type `SortedSet`. Then define the following
functions:

1.  The function `@+` that receives a sorted set and an element, and
    returns the sorted set with the element added (unless it is already
    in the sorted set).
2.  The function `setToList` that receives a sorted set and returns it
    as a list (in sorted order)
3.  The function `sortedSet` that receives a list of elements and puts
    them all in a sorted set.
4.  The function `in'` which determines if an element is in the sorted
    set.

Note that if any of your functions perform any comparison operations
(`>` etc.), you will need to include the `Ord a =>` constraint
over the elements of the sorted set or list at the beginning of the type
signature of those functions. Example runs follow:

``` haskell
ghci> setToList $ (sortedSet []) @+ 1
[1]
ghci> setToList $ (sortedSet []) @+ 1 @+ 2
[1,2]
ghci> setToList $ (sortedSet []) @+ 1 @+ 2 @+ 0
[0,1,2]
ghci> setToList $ (sortedSet []) @+ 1 @+ 2 @+ 0 @+ 2
[0,1,2]
ghci> setToList $ sortedSet [7,3,2,5,5,2,1,7,6,3,4,2,4,4,7,1,2,3]
[1,2,3,4,5,6,7]
ghci> setToList $ sortedSet "aaabccccbbbbbaaaaab"
"abc"
ghci> 1 `in'` (sortedSet [1, 2, 3])
True
ghci> 1 `in'` (sortedSet [4])
False
```

## Question 7
In this question, we are going to demonstrate an example of the *expression
problem* by writing FP-style data structures and functions, and OO-style
classes, to represent the same problem. We shall use Haskell for the FP
formulation, and Python for the OOP formulation. Ensure that your Python
code is well-typed by checking it with `pyright`.

The problem is as such. We want to represent various shapes, and the
facility to calculate the area of a shape. To start, we shall define two
shapes: circles and rectangles. Circles have a radius and rectangles
have a width and height. Assume these fields are all `Double`s
in Haskell, and `float`s in Python.

- Haskell: define a type `Shape` that represents these two
shapes, and a function `area` that computes the area of any
shape.

- Python: define a (abstract) class `Shape` that comes with a (abstract)
method `area` which gives its area. Then, define two subclasses of
`Shape` that represents circles and rectangles, and define their
constructors and methods appropriately.

The *expression problem* essentially describes the phenomenon that it
can either be easy to add new representations of a type or easy to add new
functions over types, but not both. To observe this, we are going to
extend the code we've written in the following ways:

1.  Create a new shape called `Triangle` that has a width and height.

2.  Create a new function/method `scale` that scales the shape (by
    length) by some factor \\(n\\).

Proceed to do so in both formulations. As you are doing so, think about
whether each extension is easy to do if the code we've previously
written cannot be amended, e.g. if it is in a pre-compiled library which
you do not have the source code of.

## Question 8
Let us extend our Expressions GADT.
Define the following expressions:
1.  `LitBoolExpr` holds a boolean value (`True` or
    `False`)
2.  `AndExpr` has two boolean expressions and evaluates to
    their conjunction
3.  `OrExpr` has two boolean expressions and evaluates to
    their disjunction
4.  `FuncExpr` holds a function
5.  `FuncCall` receives a function and an argument, and
    evaluates to the function application to that argument

Example runs follow:

``` haskell
ghci> n = LitNumExpr
ghci> b = LitBoolExpr
ghci> a = AndExpr
ghci> o = OrExpr
ghci> f = FuncExpr
ghci> c = FuncCall
ghci> eval (b True `a` b False)
False
ghci> eval (b True `a` b True)
True
ghci> eval (b True `o` b False)
True
ghci> eval (b False `o` b False)
False
ghci> eval $ f (\x -> x + 1) `c` n 1
2
ghci> eval $ c (c (f (\x y -> x + y)) (n 1)) (n 2)
3
```

## Question 9
In this question we shall
simulate a simple banking system consisting of bank accounts. We shall
write all this code in **Python**, but in a typed functional programming
style. That means:

1.  No loops
2.  No mutable data structures or variables
3.  Pure functions only
4.  Annotate all variables, functions etc. with types
5.  Program must be type-safe

There are several kinds of bank accounts that behave differently on
certain operations. We aim to build a banking system that receives such
operations that act on these accounts. We shall build this system
incrementally (as we should!), so you may want to follow the parts in
order, and check your solutions after completing each part.

### Bank Accounts
#### Bank Account ADT
First, create an Algebraic Data Type (ADT) called `BankAccount`
that represents two kinds of bank accounts:

1.  Normal bank accounts
2.  Minimal bank accounts

Both kinds of accounts have an ID, account balance and an interest
rate.

Example runs follow:

``` python
>>> NormalAccount("a", 1000, 0.01)
NormalAccount(account_id='a', balance=1000, interest_rate=0.01)
>>> MinimalAccount("a", 1000, 0.01)
MinimalAccount(account_id='a', balance=1000, interest_rate=0.01)
```

#### Basic Features
Now let us write some simple features of these bank accounts. There are
two features we shall explore:

1.  Depositing money into a bank account. Since we are writing code in a
    purely functional style, our function does not mutate the state of
    the bank account. Instead, it returns a new state of the account
    with the money deposited. Assume that the deposit amount is
    non-negative.
2.  Deducting money from a bank account. Just like before, we are not
    mutating the state of the bank account, and instead will be
    returning the new state of the bank account. However, the deduction
    might not happen since the account might have insufficient funds. As
    such, this function returns a tuple containing a boolean flag
    describing whether the deduction succeeded, and the new state of the
    bank account after the deduction (if the deduction does not occur,
    the state of the bank account remains unchanged).

*Note*: The type of a tuple with two elements of types `A` and
`B` is `tuple[A, B]`. Example runs follow:

``` python
>>> x = NormalAccount('abc', 1000, 0.01)
>>> y = MinimalAccount('bcd', 2000, 0.02)
>>> deposit(1000, x)
NormalAccount(account_id='abc', balance=2000, interest_rate=0.01)
>>> deduct(1000, x)
(True, NormalAccount(account_id='abc', balance=0, interest_rate=0.01))
>>> deduct(2001, y)
(False, MinimalAccount(account_id='bcd', balance=2000, 
    interest_rate=0.02))
```

#### Advanced Features
Now we shall implement some more advanced features:

1.  Compounding interest. Given a bank account with balance \\(b\\) and
    interest rate \\(i\\), the new balance after compounding will be
    \\(b(1+i)\\). For minimal accounts, an administrative fee of $20 will
    be deducted if its balance is strictly below $1000. This fee
    deduction happens **before** compounding. Importantly, bank balances
    never go below $0, so e.g. if a minimal account has $10, after
    compounding, its balance will be $0.

2.  Bank transfers. This function receives a transaction amount and two
    bank accounts: (1) the credit account (the bank account where funds
    will come from) and (2) the debit account (bank account where funds
    will be transferred to). The result of the transfer is a triplet
    (tuple of three elements) containing a boolean describing the
    success of the transaction, and the new states of the credit and
    debit accounts. The transaction does not happen if the credit
    account has insufficient funds.

Example runs follow:

``` python
>>> x = NormalAccount('abc', 1000, 0.01)
>>> y = MinimalAccount('bcd', 2000, 0.02)
>>> z = MinimalAccount('def', 999, 0.01)
>>> w = MinimalAccount('xyz', 19, 0.01)
>>> compound(x)
NormalAccount(account_id='abc', balance=1010, interest_rate=0.01)
>>> compound(compound(x))
NormalAccount(account_id='abc', balance=1020.1, interest_rate=0.01)
>>> compound(y)
MinimalAccount(account_id='bcd', balance=2040, interest_rate=0.02)
>>> compound(z)
MinimalAccount(account_id='def', balance=988.79, interest_rate=0.01)
>>> compound(w)
MinimalAccount(account_id='xyz', balance=0, interest_rate=0.01)
>>> transfer(2000, x, y)
(False, NormalAccount(account_id='abc', balance=1000,
    interest_rate=0.01), MinimalAccount(account_id='bcd', 
    balance=2000, interest_rate=0.02))
>>> transfer(2000, y, x)
(True, MinimalAccount(account_id='bcd', balance=0,
    interest_rate=0.02), NormalAccount(account_id='abc', 
    balance=3000, interest_rate=0.01))
```

#### Operating on Bank Accounts
Let us suppose that we have a dictionary whose keys are bank account IDs
and values are their corresponding bank accounts. This dictionary
simulates a 'database' of bank accounts which we can easily lookup by
bank account ID:

``` python
>>> d: dict[str, BankAccount] = {
  'abc': NormalAccount('abc', 1000, 0.01)
  'bcd': MinimalAccount('bcd', 2000, 0.02)
}
```

Now we are going to process a whole bunch of operations on this
'database'.

#### Operations ADT
The first step in processing a bunch of operations on the accounts in
our database is to create a data structure that represents the desired
operation in the first place. For this, create an algebraic data type
`Op` comprised of two classes:
1.  `Transfer`: has a transfer amount, and credit bank account
    ID, and a debit bank account ID. This represents the operation where
    we are transferring the transfer amount from the credit account to
    the debit account.
2.  `Compound`. This just tells the processor to compound all
    the bank accounts in the map. There should be no attributes in this
    class.

#### Processing One Operation
Write a function `process_one` that receives an operation and a
dictionary of bank accounts (keys are bank account IDs, and values are
the corresponding bank accounts), and performs the operation on the bank
accounts in the dictionary. As a result, the function returns a pair
containing:
1.  A boolean value to describe whether the operation has succeeded
2.  The resulting dictionary containing the updated bank accounts after
    the operation has been processed.

Take note that there are several ways in which a `Transfer`
operation may fail:
1.  If any of the account IDs do not exist in the dictionary, the transfer
    will fail
2.  If the credit account does not have sufficient funds, the transfer
    will fail
3.  Otherwise, the transfer should proceed as per normal

Keep in mind that you should not mutate any data structure used. Example
runs follow:

``` python
# data
>>> alice = NormalAccount('alice', 1000, 0.1)
>>> bob = MinimalAccount('bob', 999, 0.1)
>>> mp = {'alice': alice, 'bob': bob}

# ops
>>> c = Compound()
>>> t1 = Transfer(1000, 'alice', 'bob')
>>> t2 = Transfer(1000, 'bob', 'alice')

# processing compound operation
>>> process_one(c, mp)
(True, {'alice': NormalAccount('alice', 1100.0, 0.1), 
        'bob': MinimalAccount('bob', 1076.9, 0.1)})

# processing transfers
>>> process_one(t1, mp)
(True, {'alice': NormalAccount('alice', 0, 0.1), 
        'bob': MinimalAccount('bob', 1999, 0.1)})
>>> process_one(t2, mp)
(False, {'alice': NormalAccount('alice', 1000, 0.1), 
         'bob': MinimalAccount('bob', 999, 0.1)})
```

#### Processing All Operations
Now let us finally define a function `process_all` that
receives a list of operations and a dictionary of bank accounts (the
keys are bank account IDs, and the values are bank accounts). As a
result, the function returns a pair containing:
1.  A list of booleans where the \\(i^\text{th}\\) boolean value describes
    whether the \\(i^\text{th}\\) operation has succeeded
2.  The resulting dictionary containing the updated bank accounts after
    all the operations have been processed.

Example runs follow:

``` python
# data
>>> alice = NormalAccount('alice', 1000, 0.1)
>>> bob = MinimalAccount('bob', 999, 0.1)
>>> mp = {'alice': alice, 'bob': bob}

# op
>>> c = Compound()
>>> t1 = Transfer(1000, 'alice', 'bob')
>>> t2 = Transfer(1000, 'bob', 'alice')

# process
>>> process_all([t2, c, t2, t1], mp)
([False, True, True, True], 
 {'alice': NormalAccount(account_id='alice', balance=1100.0, interest_rate=0.1), 
  'bob': MinimalAccount(account_id='bob', balance=1076.9, interest_rate=0.1)})
```

#### Polymorphic Processing
Let us assume that your `process_all` function invokes the `process_one` function. If you were careful with your implementation of `process_all`, you _should_ be able to lift your `proces_one` function as a parameter:

```python
def process_all(ops, mp):
    # ...
    process_one(...)
    # ...

# becomes

def process_all(f, ops, mp):
    # ...
    f(...)
    # ...
```

After which, nothing about the implementation of `process_all` depends on
the types like `Op`, `dict[str, BankAccount]` or
`bool`. Thus, we should make this function polymorphic!

Our goal is to write a polymorphic function `process` that can
process any list over a state and produce the resulting list and an
updated state after performing stateful processing over the list. It
should be defined such that `process(process_one, ops, mp)`
should be the exact same as `process_all(ops, mp)` as you have
defined earlier:

``` python
# data
>>> alice = NormalAccount('alice', 1000, 0.1)
>>> bob = MinimalAccount('bob', 999, 0.1)
>>> mp = {'alice': alice, 'bob': bob}

# ops
>>> c = Compound()
>>> t1 = Transfer(1000, 'alice', 'bob')
>>> t2 = Transfer(1000, 'bob', 'alice')

# process
>>> process(process_one, [t2, c, t2, t1], mp)
([False, True, True, True], 
 {'alice': NormalAccount(account_id='alice', balance=1100.0, interest_rate=0.1),
  'bob': MinimalAccount(account_id='bob', balance=1076.9, interest_rate=0.1)})
```

Furthermore, the best part of this polymorphic function is that it can
be used in any situation where we need this stateful accumulation over a
list. For example, we can define a function that tests if a number \\(n\\)
is co-prime to a list of other numbers, and if it is indeed co-prime to
all of the input numbers, add \\(n\\) to the state list:

``` python
>>> def gather_primes(n: int, ls: list[int]) -> tuple[bool, list[int]]:
...     if any(n % i == 0 for i in ls):
...         return (False, ls)
...     return (True, ls + [n])
```

Example uses of this follow:

``` python
>>> gather_primes(2, [])
(True, [2])
>>> gather_primes(3, [2])
(True, [2, 3])
>>> gather_primes(4, [2, 3])
(False, [2, 3])
```

This way, we can use `process` to generate prime numbers and do
primality testing!

``` python
>>> def primes(n: int) -> tuple[list[bool], list[n]]:
...     return process(gather_primes, list(range(2, n)), [])
... 
>>> primes(10)
([True, True, False, True, False, True, False, False], [2, 3, 5, 7])
>>> primes(30)
([True, True, False, True, False, True, False, False, False, # 2 to 10
  True, False, True, False, False, False, True, False, True, # 11 to 20
  False, False, False, True, False, False, False, False, False, True], 
  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29])
```

Proceed to define the `process` function. Example runs are as
above.

*Note*: The type of a function that receives parameters `A`, `B` and `C`
and returns `D` is `Callable[[A, B, C], D]`. You will need to
import `Callable` from `typing`.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
