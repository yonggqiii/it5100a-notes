![Updated][update-shield]
# Functional Programming

*Functional Programming* (FP) is a *declarative* *programming paradigm* where *functions* take centre stage. As a recap from IT5001, you might have learnt that programming paradigms are schools of thought for writing programs. IT5001 has very likely exposed you to *imperative* paradigms like *procedural* and *Object-Oriented Programming*. The following table shows other popular programming paradigms:

| Imperative | Declarative|
| --- | --- |
|Procedural | Logic|
|Object-Oriented | Functional|

Object-Oriented Programming (OOP) has four principles as you might recall: *Abstraction*, *Inheritance*, *Encapsulation* and
*Polymorphism*.[^1] Functional Programming, on the other hand, is centered around the following principles, which really are just principles of mathematical functions and the \\(\lambda\\) *calculus*:[^4] 

-   Immutability
-   Pure Functions
-   Recursion
-   Types
-   First-Class Functions

Let's briefly describe what these principles entail.

## Immutability

The idea of *immutability* is simple&mdash;only use **immutable** data. For example, the following program fragment does not perform any mutation, not even on the variables:

``` python
def add_one(fraction):
    """fraction is a tuple of (numerator, denominator)"""
    old_num, den = fraction
    num = old_num + den
    return (num, den)
 
my_fraction = (3, 2)
new_fraction = add_one(my_fraction)
 
print(new_fraction) # (5, 2)
print(my_fraction) # (3, 2)
```

The fact that the program does not perform any mutation makes this very
similar to mathematical functions where mathematical objects are seen as
values instead of references to cells that can be changed. This makes
reasoning about any of the variables, objects and functions incredibly
simple.

Overall, immutability forces us to be disciplined with **state**.
Contrast this with using **mutable** data structures and variables, such as in the following program fragment:

``` python
def f(ls):
  ls[0] = 4
  return ls
 
my_ls = [1, 2, 3]
print(f(my_ls)) # [4, 2, 3]
print(my_ls) # [4, 2, 3]
```

This is one of the classic examples of the problems with mutability&mdash;it is
not at all clear whether passing a list into a function will preserve
the state of the list. Because lists are mutable, we have no guarantee
that functions or any operation will not cause the *side-effect* of
mutation (accidental or intentional).


## Pure Functions

Just like mathematical functions, functions (in programming) should be
*pure*. Pure functions really look like mathematical functions, for
example, \\(f\\) below: 

\\[f: \mathbb{N} \to \mathbb{N}\\]
\\[f(x) = x^2 + 2x + 3\\] 

An equivalent implementation in Python would
look like:

``` python
def f(x):
  return x ** 2 + 2 * x + 3
```

Pure functions **only receive input and return
output**. They do not produce side effects, and do not depend
on external state. And example of this is as follows:

``` python
# Python
def double(ls):
  return [i * 2 for i in ls]
 
x = [1, 2, 3]
 
print(double(x)) # [2, 4, 6]
print(double(x)) # [2, 4, 6]
print(double(x)) # ...
# ...
```

Notice that the `double` function is pure! In this example,
`double(x)` evaluates to `[2, 4, 6]`; thus,
`double(x)` and `[2, 4, 6]` are the **same**! This
property of pure functions is known as *referential transparency*, and
makes reasoning about and optimizing programs much more straightforward.

Contrast the behaviour of pure functions with that of impure functions:

``` python
def f():
  global ls
  x = ls # use of global variable
  addend = x[-1] + 1
  x.append(addend) # is there a side-effect?
  ls = x + [addend + 1] # mutate global variable
  return ls
 
ls = [1, 2, 3]
x = ls
 
print(f()) # [1, 2, 3, 4, 5]
print(ls) # [1, 2, 3, 4, 5]
print(x) # [1, 2, 3, 4]
```

So many side effects have been caused! Functions like these make
reasoning about program behaviour incredibly difficult. Converting this
function into a pure one (removing all side-effects) makes its behaviour
clearer and more transparent.

``` python
def f(ls):
  x = ls
  addend = x[-1] + 1
  x = x + [addend]
  ls = x + [addend + 1]
  return ls
 
ls = [1, 2, 3]
x = ls
 
print(f(ls)) # [1, 2, 3, 4, 5]
print(ls) # [1, 2, 3]
print(x) # [1, 2, 3]
```
## Recursion

You have seen this before&mdash;use *recursive* functions to simulate loops.[^2] Let's look at an example of a perfectly reasonable way to sum
the numbers of a 2-dimensional list, using the `sum2D` function:

``` python
def sum2D(ls):
  total = 0
  for row in ls:
    for num in row:
      total += num
  return total
```

Loops are typically useful for its side-effects, primarily mutation.
Looking at the (nested) loop above, a bunch of mutation occurs: the
reassignments to `row` and `num` (the loop variables),
and the mutation of the `total` variable in the loop body. In
an environment where mutation is impossible, can we write the same
program? Yes! Like we have said, rely on **recursion**! An example
recursive formulation of the `sum2D` function from above would
be like so:

``` python
def row_sum(row):
    return 0 if not row else \
           row[0] + row_sum(row[1:])
 
def sum2D(ls):
    return 0 if not ls else \
           row_sum(ls[0]) + sum2D(ls[1:])
```

Again, the behaviour of the program has not changed: the
`sum2D` function still produces the correct output given any
2-dimensional list of integers. However, our function is still pure and
does not mutate **any** data structure or variable.

Recursive solutions can also be more elegant,
especially when the problem or data structures used are (inherently)
recursive. Take the example of obtaining the preorder of a binary tree.
Binary trees are recursive data structures, if formulated the following
way:

> A (nonempty) binary tree is either:
>    - A node with a value, a left tree and a right tree; OR
>    - A leaf with just a value

As you can see, the definition of a node contains (sub)trees, making the binary tree
a recursive data structure[^3]. Therefore, operations on trees can often be
expressed elegantly using recursion. For example, the specification of
obtaining the *preorder* of a tree can be like so:

1.  The preorder of a leaf is a list containing the leaf's value

2.  The preorder of a node is the node's value, together with the
    preorder of the left (sub)tree, then the preorder of the right
    (sub)tree.

This specification written in code is concise and elegant:

``` python
from dataclasses import dataclass
 
@dataclass
class Tree: pass
 
@dataclass
class Node(Tree):
    val: object
    left: Tree
    right: Tree
 
@dataclass
class Leaf(Tree):
    val: object
 
def preorder(tree):
    match tree:
        case Node(val=v, left=l, right=r):
            return [v] + preorder(l) + preorder(r)
        case Leaf(val=v):
            return [v]
```

Recursive functions are also amenable to **formal** reasoning. Some
languages (usually *Interactive Theorem Provers*) support proofs and can
even automatically synthesize proofs of correctness for you. In the
following example written in Lean 4, the following program defines a
binary tree and a program for obtaining the preorder of the tree just as
before; the key difference being, that Lean automatically helps us prove
that the function **terminates**. In such an environment, we rarely have
to worry whether our program gets stuck or crashes.

``` lean
inductive Tree (α : Type) : Type where
  | node : α -> Tree α -> Tree α -> Tree α
  | leaf : α -> Tree α 
 
-- compiler automatically synthesizes proof of termination
def Tree.preorder { β : Type } : Tree β -> List β
  | .node v l r => v :: (preorder l) ++ (preorder r)
  | .leaf v => [v]
 
def myTree : Tree Nat := .node 1 (.leaf 2) (.leaf 3)
#eval myTree.preorder -- [1, 2, 3]
```

The primary reason for this is that recursive functions can often be
reasoned about via *induction*: 

\\[\frac{P(0)~~~~~~~~\forall k \in \mathbb{N}. P(k)\to P(k + 1)}{\forall n \in \mathbb{N}. P(n)} \\text{Induction}\\]

We have seen that factorial can be written recursively, and in fact we
can prove its correctness (in a quite straightforward manner) via
induction. This makes the following factorial function implementation
obviously correct.

``` lean
-- Lean 4
def fac : Nat -> Nat 
  | 0     => 1
  | n + 1 => (n + 1) * fac n
```

### Types

Adhering strictly to type information **eliminates type-related bugs**
and makes functions **transparent**. Perhaps most importantly, adherence
to type information can be verified by a program.

Observe the following program fragment.

``` python
x: int = 123
# ...
print(x + 5)
```

If we fix the type of `x` to `int` and strictly adhere
to it, then the last line containing `x + 5` will definitely
not cause a `TypeError`, because we know that adding any number
to an integer will always work.

Contrast the above with the following example.


``` python
# Python
def safe_div(num: int, den: int) -> int:
    return None if den == 0 else \
           num // den
 
x = int(input())
y = int(input())
z = safe_div(x, y) + 1 # hmmm...
print(z)
```

If we do not adhere to typing information strictly, no one knows that
the `safe_div` function could return `None`! In such a
scenario, if the user enters `0` for `y`, the
expression `safe_div(x, y) + 1` would give a
`TypeError`!

Function purity and adhering to types forces functions to be
**transparent in effects**. That is because if we want our pure function
to perform some effectful computation (such as potentially returning
`None`), we must return an object that encapsulates this
behaviour; coupled with adhering to types, we must assign the correct
type for the output of the function&mdash;the type of the object which
encapsulates this behaviour&mdash;making the function's effects obvious. 

To improve the program written earlier, let us try to create a data structure
`Maybe` that is one of two things: `Just` a value, or
`Nothing`. We can express this as dataclasses in Python (you
may ignore the stuff involving `typing` and all the square brackets
for now, they will make sense later).

```python
from typing import Any
from dataclasses import dataclass

@dataclass(frozen=True)
class Maybe[T]:
    """Represents computation that may result in nothing"""
    pass
 
@dataclass(frozen=True)
class Just[T](Maybe[T]):
    j: T
 
@dataclass(frozen=True)
class Nothing(Maybe[Any]):
    pass
```

Now we can amend our `safe_div` function appropriately to
return a `Maybe` value:

``` python
def safe_div(num: int, den: int) -> Maybe[int]:
    return Nothing() if den == 0 else \
           Just(num // den)
```

Notice two things: 1) the function is pure, and does nothing other than
receive inputs and returns output 2) the function's type signature makes
it incredibly obvious that the function will *maybe* produce an
`int`. Therefore, users of this function are *forced* to handle
the case where the function produces `Nothing`.

From this, we may proceed to use the `safe_div` function as
before, except that instead of directly assigning
`z = safe_div(x, y) + 1`, we must first call
`safe_div` and handle the two cases: one where some integer was
returned, the other where nothing was.

```python
x: int = int(input())
y: int = int(input())
z: Maybe[int]
match safe_div(x, y):
    case Just(j):
        z = Just(j + 1)
    case Nothing():
        z = Nothing()
```

Types and type systems are highly useful, not just for verification of
type safety, but also more generally, program verification and theorem
proving etc. Types are backed by a rich theory (type theory) and is
widely studied. As an example, interactive theorem provers may rely
on systems with advanced type systems (such as the calculus of constructions, which has
*dependent types*) to form the computational basis for proof assistance
and proof checking. When these systems are baked into the language, we
can write proof-carrying code and theorems (mathematical theorems or
theorems about properties of code itself). An example is as follows,
where theorems about the additive identity and the commutativity of
addition of numbers can be used to show that concatenating a vector
(like an immutable list) of length \\(n\\) to one of length \\(k\\) gives a
vector of length \\(n + k\\).

``` lean
-- Lean 4
theorem izero : ∀ (k : Nat) , k = 0 + k
  | 0 => by rfl
  | n + 1 => congrArg (. + 1) (izero n)
 
theorem isucc (n k : Nat) : n + k + 1 = n + 1 + k :=
  match k with 
  | 0 => by rfl
  | x + 1 => congrArg (. + 1) (isucc n x)
 
def Vect.concat {α : Type} {n k : Nat} : Vect α n -> Vect α k -> Vect α (n + k)
  | .nil, ys => izero k ▸ ys
  | .cons x xs, ys => isucc _ _ ▸ .cons x (xs.concat ys)
```

### First-Class Functions

You might have seen in IT5001 that in some languages, functions are
*first-class* objects.[^7] This gives rise to higher-order functions which
support **code re-use**. *Higher-order functions* can receive functions
as arguments and/or return functions as output. 

In the following program fragment, the `map` method of
`Tree`s receive a function and returns a new tree with the
function applied to all of its values. We then also *curry* the
`add` function so that it receives the first addend, then
returns a function that receives the second addend and returns the sum.
This way, adding 2 to the values of a tree is as simple as several
function calls:

``` python
@dataclass(frozen=True)
class Tree:
    def map(self, f):
        match self:
            case Leaf(v):
                return Leaf(f(v))
            case Node(v, l, r):
                newval = f(v)
                newl = l.map(f)
                newr = r.map(f)
                return Node(newval, newl, newr)
 
@dataclass(frozen=True)
class Node(Tree):
    val: object
    left: Tree
    right: Tree
 
@dataclass(frozen=True)
class Leaf(Tree):
    val: object
 
def add(x):
    return lambda y: x + y
 
x = Node(1, Leaf(2), Leaf(3))
print(x.map(add(2))) # Node(3, Leaf(4), Leaf(5))
```

Functional programming languages emphasize this fact and make it easy
and ergonomic to define higher-order functions. For example, in Haskell,
functions are automatically curried, and has higher-order functions like
`map` built into the standard library. This makes, for
example, adding two to elements of a list, straightforward:

```haskell
main :: IO ()
main = do
  let x = [1, 2, 3]
  print (map (+2) x) -- [3, 4, 5]
```

### So what?

Ideas from functional programming languages are increasingly being
adopted in commonly-used imperative programming languages:

-   Closures in C++/Rust/Java 8

-   Structural pattern matching in Python 3.11/Java 21

-   Algebraic Data Types in Rust

-   Records in Java 14 etc.

Learning functional programming has a direct impact on your future work as a developer; functional programming is more than just a collection of language features and principles&mdash;it fundamentally encourages a new way of solving problem. As we’ve discussed, some of these principles impose meaningful constraints on programmers, which can make problem-solving more challenging and require innovative strategies. Nevertheless, mastering functional programming is invaluable, as it offers a fresh perspective on problem-solving. The skills you acquire will not only enhance your discipline as a developer but also empower you to explore diverse approaches to the challenges you encounter in your daily work.

Our goal for this course is to therefore first learn how to write
programs in a purely functional programming language (thus forcing you
to write programs fully with FP), and then transfer concepts into
commonly used programming languages. For this, we will be writing code
in two languages: *Haskell* (a purely functional programming language)
and Python (which you should all be relatively familiar with).

### Things You Need
For this course, you will need the following software:

-   The Glasgow Haskell Compiler (GHC) (recommended: GHC 9.4.8 or newer)

-   Python 3.12 (note the version; we shall be using new features)

-   Any text editor you like (Visual Studio Code, Neovim etc.)


--- 
[^1]: Polymorphism in OOP refers to *subtype polymorphism*, which is
    different to the polymorphism in FP known as *parametric
    polymorphism*.

[^4]: If you have not, you may want to read [a recap on the \\(\lambda\\) calculus](../../recap/sections/lambda.md) before continuing.

[^2]: If you have not, you may want to read
    [a recap on recursion](../../recap/sections/recursion.md) before continuing.

[^3]: (Singly-linked) lists are also recursive data structures. To see
    this, look at our definition of binary trees, and remove one subtree
    in the definition of a node (therefore, a node has a value and one
    subtree). This is now a singly-linked list.

[^7]: If you have not, you may want to read a 
    [a recap on first-class functions](../../recap/sections/first-class-functions.md) before continuing.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20SEP%202024-57ffd8?style=for-the-badge
