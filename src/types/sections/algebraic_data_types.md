![Updated][update-shield]

# Algebraic Data Types

We have just seen different data types in Haskell, and introduced the
concept of polymorphic types as demonstrated by examples in Python. Yet,
we have not discussed how we can create our own (polymorphic) data types
in Haskell!

Haskell is a purely functional language, so do not expect classes here.
In OOP, objects have both data (attributes) and behaviour (methods),
whereas this is not necessarily a principle in FP (although, you can have data
types with functions as fields since functions are first-class). We already know how to create functions, so now we must investigate how we can create data types in a
purely functional language.

If we think about it carefully, we might realize that data types are a
mix of the following:

-   A type **and** another type **and**...**and** yet another type

-   A type **or** another type **or**...**or** yet another type

We can express the following types using **and** and **or** over other
types:

-   A `Fraction` consists of a numerator (`Int`) **and** a denominator
    (`Int`)

-   A `Student` consists of a name (`String`) **and** an ID (`Int`)

-   A `Bool` is either `True` **or** `False`

-   A `String` is either an empty string **or** (a head character
    (`Char`) **and** a tail list (`String`))

-   A polymorphic `Tree` is either (a leaf with a value of type `a`)
    **or** (a node with a value (`a`) **and** a left subtree (`Tree a`)
    **and** a right subtree (`Tree a`))

This formulation of data types as products (**and**) and/or sums
(**sum**) is what is known as Algebraic Data Types (ADTs) (not to be confused
with Abstract Data Types). In Haskell, types are **sums** of zero or more
**constructors**; constructors are **products** of zero or more types.

To create a new data type in Haskell, we can use the `data`
keyword. Let us create a fraction type based on our algebraic
specification above:

``` haskell
data Fraction = Fraction Int Int

half :: Fraction
half = Fraction 1 2
```

On the left hand side we have the declaration of the type, and on the
right hand side, a list of constructors separated by `|` that help us create the type.
Note that the `Fraction` on the right hand side is the name of
the constructor of the type; it in fact can be distinct from the name of
the type itself (which is very helpful when you have more than one
constructor). As you can see, to construct a `Fraction` (the type), the
`Fraction` _constructor_ receives two `Int`s, one
numerator, and one denominator.

Then, defining the student type from our algebraic formulation above
should also be straightforward:

```haskell
data Student = S String Int

bob :: Student
bob = S "Bob" 123
```

Let us define the `Bool` type, which should have two constructors, each
constructor not having any fields:

```haskell
data Bool = True | False

true, false :: Bool
true = True
false = False
```

To construct a `Bool` we can use either the `True`
constructor or the `False` constructor. Neither of these
constructors receive any other fields.

We can also have multiple constructors that are products of more than
zero types, as we shall see in the algebraic formulation of a
`String`:

```haskell
data String = EmptyString | Node Char String

hello, empty :: String
hello = Node 'h' (Node 'e' (Node 'l' (Node 'l' (Node 'o' EmptyString))))
empty = EmptyString
```

## Polymorphic Algebraic Data Types

Now we show examples of creating our own polymorphic data types. The way
we would do so is similar to how we defined generic/polymorphic classes
in Python.

Let us start from the bottom again by creating specialized versions of a
box type, this time in Haskell. We start by assuming that a box contains
an `Int`:

```haskell
data IntBox = IB Int
b :: IntBox
b = IB 1
```

Then define a box that contains a `String`:

```haskell
data StrBox = SB String
b :: StrBox
b = SB "123"
```

Again, they look more or less the same, except for the type of the
field. As such, we should allow `Box` to be polymorphic by
introducing a type parameter:

```haskell
data Box a = B a
x :: Box Int
x = B 1
y :: Box String
y = B "123"
```

Perfect! Let us try more complex polymorphic algebraic data types like
linked lists and trees:

```haskell
data LinkedList a = EmptyList | Node a (LinkedList a)
cat :: LinkedList Char
cat = Node 'c' (Node 'a' (Node 't' EmptyList))

data Tree a = Leaf a | TreeNode (Tree a) a (Tree a)
tree :: Tree Int
tree = TreeNode (Leaf 1) 2 (Leaf 3)
```

Constructors are actually functions!

```haskell
ghci> data Fraction = F Int Int
ghci> :t F
F :: Int -> Int -> Fraction
ghci> :t F 1
F 1 :: Int -> Fraction
ghci> :t F 1 2
F 1 2 :: Fraction
```

We now have the facilities to define and construct data types and their
terms, but so far we are not able to *access* the fields of a data type
in Haskell. Unlike Python, we are not able to do something like
`x.numerator` to obtain the numerator of a fraction `x`, for example.
There are ways to define functions that do so and we will show them to
you in later sections, but for now, Haskell has *record syntax* that
automatically defines these accessor functions for us.

Let us re-create the `Student` type, this time using record
syntax to automatically derive functions that obtain their names and
IDs:

```haskell
data Student = S { name :: String, id :: Int }
```

With this, we no longer need to define our own functions that access
these fields for us. Record syntax is great for giving names to fields!
Importantly, record syntax is nothing special, and we can continue to
create terms of those types by way of usual constructor application.

```haskell
x, y :: Student
x = S { name = "Alice", id = 123 }
y = S "Bob" 456
```

Let's try loading this into GHCI and see the accessor functions in
action:

```haskell
ghci> name x
"Alice"
ghci> id y
456
```

You can also make use of record syntax to express record updates. For example, 
we can update Alice to have the ID of 456 like so:
```haskell
ghci> id x
123
ghci> z = x { id = 456 }
ghci> name z
"Alice"
ghci> id z
456
```
Of course, the original term was not actually _updated_ since everything is immutable in Haskell&mdash;`x { id = 456 }` simply constructs a new term that contains the same values for all its fields, except where the `id` field now takes the value `456`.


We can even mix and match these different forms of constructor
definitions, or create large data structures!

```haskell
data Department = D {name' :: String, courses :: [Course]}
data Course = C { code :: String, 
                  credits :: Int,
                  students :: [Student] }
data Student = UG { homeFac :: String,
                    name :: String,
                    id :: Int }
             | PG [String] String Int

alice   = UG "SoC" "Alice" 123
bob     = PG ["SoC", "YLLSoM"] "Bob" 456
it5100a = C "IT5100A" 2 [alice]
it5100b = C "IT5100B" 2 [alice, bob]
cs      = D "Computer Science" [it5100a, it5100b]
```

## More on Polymorphism

Now that we have shown how to create our own algebraic data types in
Haskell (and polymorphic ones), we step aside and give a mental model
for understanding polymorphism. Recall that we have described
polymorphic functions and types as functions/types that
quantifies/parameterizes types; in other words, they receive a type as a
parameter.

Recall in the lambda calculus that \\(\lambda\\) creates a function over a
parameter. Assuming the parameter has type \\(S\\) and the returned value
has type \\(T\\), we get: 

$$\lambda x.e: S \to T$$ 

and when we call or apply
this function, we are substituting the parameter for the argument of the
function application: 
$$(\lambda x.e_1)e_2 \equiv_\beta e_1[x:=e_2]$$

$$\begin{aligned}
(\lambda x: \mathtt{Int}.x + 4)3 &\equiv_\beta (x + 4)[x := 3]\\\\
&\equiv_\beta (3 + 4)\\\\
& \equiv_\beta 7
\end{aligned}$$ In Haskell (the expression in parentheses is a lambda
expression):

``` haskell
ghci> (\x -> x + 4) 3
7
```

A typed variant of the lambda calculus known as System \\(F\\) has
polymorphic functions, which are functions that also receive a type
parameter. We can then apply this function onto a type *argument* to get
a specialized version of that function. Such type parameters are bound
by \\(\Lambda\\). As an example, if we have a term \\(e\\) of type \\(T\\), we get:

$$\Lambda \alpha.e: \forall\alpha.T$$
Calling or applying this function with a type argument, once again, substitutes the type parameter with the type argument:

$$(\Lambda\alpha.e)\ \tau\equiv_\beta e[\alpha := \tau]$$
$$(\Lambda\alpha.e)\ \tau : T[\alpha := \tau]$$

$$\begin{aligned}
(\Lambda \alpha.\lambda x:\alpha.[x]) \mathtt{Int} &\equiv_\beta (\lambda x:\alpha.[x])[\alpha := \mathtt{Int}]\\\\
  & \equiv_\beta \lambda x:\mathtt{Int}.[x]
\end{aligned}$$

We can show this with an example in Haskell. Explicit type arguments
must be enabled with a language extension and the type arguments must be
prefixed by `@`:

``` haskell
ghci> :set -XTypeApplications -fprint-explicit-foralls
ghci> :{
ghci| f :: forall a. a -> [a]
ghci| f x = [x]
ghci| :}

ghci> :t f
f :: forall a. a -> [a]

ghci> :t f @Int
f @Int :: Int -> [Int]

ghci> f @Int 1
[1]
```

On the other hand, polymorphic types can be seen as *functions at the
type-level*. These are "functions" that receive types and return types!
For example, we can define a `Pair` type that is polymorphic
in its component types. Thus, the `Pair` type itself (not its
constructor!) receives two types, and returns the resulting
`Pair` type specialized to those component types. This makes
`Pair` what is known as a *type constructor*.

To observe this fact, know that types are to terms as *kinds* are to
types: they describe what *kind* of type a type is. The usual types that
we encounter `Int`, `[[Char]]` etc. have kind
`*`, and type constructors or "type-level functions\" have
kind `* -> *` for example. Below, we show that
`Pair` is a type constructor of kind `* -> * -> *`,
which makes sense since it receives two types and returns the
specialized type of the `Pair`:

``` haskell
ghci> data Pair a b = P a b
ghci> :k Pair
Pair :: * -> * -> *
ghci> :k Pair Int
Pair Int :: * -> *
ghci> :k Pair Int String
Pair Int String :: *
```

We know that we can have higher-order functions, for example, the type
of `map` might be something like
`(a -> b) -> [a] -> [b]`. Can we have higher-order type
constructors? Yes! These are known as *higher kinds* or *higher-kinded
types*. These types receive *type constructors* as type arguments. Let
us construct a higher-kinded type that receives a type constructor and
applies it onto a type:

``` haskell
ghci> data Crazy f a = C (f a)
```

Upon visual inspection we can see that `f` must be a type constructor,
because the constructor `C` receives a term of type `f a`!
What's crazier is, when inspecting the kind of `Crazy`, we see
that it exhibits *kind polymorphism*:

``` haskell
ghci> :set -fprint-explicit-foralls
ghci> :k Crazy
Crazy :: forall {k}. (k -> *) -> k -> *
```

To give you an example of how this might work, because we know we can
construct lists of any type, `[]` (the type, not the empty
list) must be a type constructor. We can thus pass the `[]`
type constructor into `Crazy`:

``` haskell
ghci> :k Crazy []
Crazy [] :: * -> *
ghci> :k Crazy [] Int
Crazy [] Int :: *
```

How might this work? We see that `Crazy [] Int` has kind `*`,
so we should be able to construct a term of this type. We can do so by
using the `C` constructor defined above! To be clear, let's
see the specialized version of the constructor with the type arguments
entered:

``` haskell
ghci> :t C @[] @Int
C @[] @Int :: [Int] -> Crazy [] Int
```

As we can see, to construct a term of this type, we just need to pass in
a list of integers to `C`:

``` haskell
ghci> x :: Crazy [] Int = C [1]
```

We can in fact instantiate other crazy types with different type
constructors:

``` haskell
ghci> data Box a = B a
ghci> y :: Crazy Box Int = C (B 2)
```

The utility of higher-kinded types may not be apparent to you now; later
on we might see some of them in action!

Although this might confuse you so far, what we have demonstrated merely
serves to demonstrate the idea that parametric polymorphism can be
thought of the phenomenon where something (type or term) can receive a
type and give you a type or term, just as we have stated at the
beginning of [Chapter 2.2 (Polymorphism)](./polymorphism.md).

## Other Polymorphisms

At the start of [Chapter 2.2 (Polymorphism)](./polymorphism.md) we introduced three questions, two of
which have been answered. Let us restate the final question and pose one
more:

1.  Can types depend on terms?

2.  Are there other kinds of polymorphism?

The answers to both questions is yes. Types that depend on terms are
known as *dependent types*, which we shall not cover in this course.
There are also other kinds of polymorphisms, some of which you have
already dealt with. Subtype polymorphism is used frequently in OOP,
since subclasses are types that are *subtypes* of their superclasses. An
umbrella term *ad-hoc polymorphism* generally refers to *overloading*,
which we shall discuss in the future. There are also more kinds of
polymorphisms, but we shall not discuss them in this course.

Python (and several other mainstream languages) is quite special, being
a multi-paradigm language means that several forms of polymorphism are
applicable to it. In particular, we have seen that Python supports
parametric polymorphism, and since Python supports OOP, it also has
subtype polymorphism. Despite Python not having algebraic data types
(yet), we may also formulate our types to behave similarly to Algebraic
Data Types. Two formulations we may attempt are: 1) with types as unions
and constructors as classes, 2) with types as classes and constructors
as their subclasses. Below we present both formulations for the linked
list type:

``` python
# (1)
type List[a] = Node[a] | Empty

@dataclass
class Empty:
    pass

@dataclass
class Node[a]:
    head: a
    tail: List[a]

x: List[int] = Node(1, Node(2, Empty()))
```

``` python
# (2)
from typing import Any
@dataclass
class List[a]:
    pass

@dataclass
class Empty(List[Any]):
    pass

@dataclass
class Node[a](List[a]):
    head: a
    tail: List[a]

x: List[int] = Node(1, Node(2, Empty()))
```

There are some differences between the two formulations, and between
these with Haskell's Algebraic Data Types. Most importantly, in Haskell,
data types are types, but constructors are not. This is unlike Python,
where all classes are types. That means a variable of type
`Node[int]` is valid in Python, but a variable of type
`Node Int` is not in Haskell.

## Generalized Algebraic Data Types

However, something interesting is going on here. In the second
formulation, a `Node[a]` is a `List[a]`, which makes sense. On
the other hand, an `Empty` can be typed as `List[Any]`, because an empty
list fits all kinds of lists. An interesting observation you might see
is that the supertype of our "constructors" need not strictly be
`List[a]`, it could be any kind of list!

Consider the following example of defining simple expressions in a programming
language, which is defined polymorphically using OOP:

``` python
class Expr[a]:
    def eval(self) -> a:
        raise Exception
```

The `Expr` class is parameterized by the type of its evaluation. From
this class we may now create subclasses of `Expr`. For example, some
simple numeric expressions.

``` python
@dataclass
class LitNumExpr(Expr[int]):
    n: int
    def eval(self) -> int:
        return self.n

@dataclass
class AddExpr(Expr[int]):
    lhs: Expr[int]
    rhs: Expr[int]
    def eval(self) -> int:
        return self.lhs.eval() + self.rhs.eval()
```

We can then create other kinds of expressions. For example, an equality
expression that returns booleans:

``` python
@dataclass
class EqExpr[a](Expr[bool]):
    lhs: Expr[a]
    rhs: Expr[a]
    def eval(self) -> bool:
        return self.lhs.eval() == self.rhs.eval()
```

Or even a conditional expression whose evaluated type is parameterized:

``` python
@dataclass
class CondExpr[a](Expr[a]):
    cond: Expr[bool]
    true: Expr[a]
    false: Expr[a]
    def eval(self) -> a:
        return self.true.eval() if self.cond.eval() else self.false.eval()
```

Let's try this out! Suppose we would like to evaluate the following
expression:

```
if 1 == 2 then 1 + 1 else 0
```

Let's write this in the program using our classes and evaluate it!

``` python
zero: Expr[int] = LitNumExpr(0)
one: Expr[int] = LitNumExpr(1)
two: Expr[int] = LitNumExpr(2)
one_plus_one: Expr[int] = AddExpr(one, one)
one_eq_two: Expr[bool] = EqExpr(one, two)
cond: Expr[int] = CondExpr(one_eq_two, one_plus_one, zero)
print(cond.eval()) # 0
```

How do we create such an algebraic data type in Haskell? For this, we
have to use *Generalized Algebraic Data Types* (GADTs). Loosely, these
are algebraic data types like before, except that each constructor can
decide what type it returns!

First, let us formulate our original algebraic data types using GADT
syntax.

``` haskell
data LinkedList a where
    EmptyList :: LinkedList a -- this is a different a!
    Node :: b -> LinkedList b -> LinkedList b
```

Now let us take it a step further, and truly customize the constructors
of an `Expr` GADT:

``` haskell
data Expr a where
    LitNumExpr :: Int -> Expr Int
    AddExpr    :: Expr Int -> Expr Int -> Expr Int
    EqExpr     :: Expr a -> Expr a -> Expr Bool
    CondExpr   :: Expr Bool -> Expr a -> Expr a -> Expr a
```

Pretty neat huh! There are many uses of GADTs, and we might see them in
the future. In the next section, we will show you how we can write
functions against algebraic data types and GADTs, including how we can
implement the `eval` function.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
