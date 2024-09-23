# Polymorphism {#sec:polymorphism}

In FP, functions describe computation and applying functions perform
said computation. For example, given a function $f$:
$$f(x) = x \times 2$$ $f$ describes what computation is to be done
(multiplying the parameter by 2), and applying $f$ onto a value (such as
$f(2)$) performs the computation that gives the result, which is $4$.
Importantly, you might also find that applying it onto a different input
may give you a different outcome. In this case, $f(2)=4\neq f(3)=6$. The
output depends on the input, i.e. we have *terms that depend on terms*.
This may at first glance seem like a trivial observation because that is
what functions are designed to do: if functions are always constant like
$g(x) = 1$ then we can always replace all applications of the function
with the result and no computation needs to be done.

However, now that we have learnt about types, we get a much more
interesting avenue for extending this idea of dependence. In fact, we
now have three orthogonal directions to explore[^2]:

1.  Can terms depend on types?

2.  Can types depend on types?

3.  Can types depend on terms?

The answer to the first two questions is yes! This phenomenon is known
as \[parametric\] *polymorphism*, i.e. where types and terms can depend
on types[^3].

### Polymorphic Types

Let us motivate this need with an example. Suppose we are trying to
create a wrapper class called `Box`{.python}, that contains a single
value. As per usual, we have to think about the type of the value it
contains. At this point we cannot simply allow the value to be
*anything*, so we shall fix the type of the value to something, say,
`int`{.python}.

``` python
# Python
@dataclass
class IntBox:
    value: int
```

However, we may later want a `Box`{.python} that stores strings. In this
case, we will have to define a new class that does so.

``` python
# Python
@dataclass
class StrBox:
    value: str
```

Recall one of the core principles in programming: whenever you see a
pattern in your code, *retain similarities* and *parameterize*
differences. Looking at the two `Box` implementations, you should be
able to see that the implementation is virtually identical, and the only
difference is the *type* of `value`. We have previously been able to
parameterize values (regular function parameters), parameterize
behaviour (higher-order functions), however, can we parameterize
*types*?

Yes! We can define `Box` to receive a *type parameter* `a`, and allow
the value in the box to be of that type `a`[^4].

``` python
# Python 3.12
@dataclass
class Box[a]:
    value: a
```

This class is a generalized `Box` class that can be *specialized* into a
specific `Box`. For example, by replacing `a` with `int`{.python} then
we recover our `IntBox`{.python} class with an `int`{.python} value;
replacing `a` with `str`{.python} recovers our `StrBox`{.python} class
with a `str`{.python} value.

``` python
x: Box[int] = Box[int](1)
y: Box[str] = Box[str]('a')
z: Box[Box[int]] = Box(Box(1))
bad: Box[int] = Box[int]('a')
```

In Python and many Object-Oriented languages, `Box` is called a
*generic* or *parametrically polymorphic* class/type. This is one
example of a *type depending on a type*.

### Polymorphic Functions

The same principle can be applied to *terms depending on types*. Suppose
we have a function `singleton` that is to receive an object and puts
that object in a list. In the same vein, we have to decide what the type
of the parameter is, which dictates the corresponding return type. For
example, may define this function that works on `int`{.python}s, and
separately, another function that works on `str`{.python}s:

``` python
# Python 3.12
def singleton_int(x: int) -> list[int]:
    return [x]
def singleton_str(x: str) -> list[str]:
    return [x]
```

Once again, we can observe that the implementations of these functions
are identical, and only the types are different. Let us combine these
implementations into a single function where the types are
parameterized!

``` python
# Python 3.12
def singleton[a](x: a) -> list[a]:
    return [x]
x: list[int] = singleton(1)
y: list[str] = singleton('a')
bad: list[bool] = singleton(2)
```

`singleton` is what is known as a polymorphic function, where the
function depends on the type!

### Polymorphic Functions in Haskell

How would we define the type of polymorphic functions in Haskell? That
is pretty straightforward: type parameters are lowercase. For example,
the `singleton` function can be defined like so:

``` haskell
singleton :: a -> [a]
singleton x = [x]
```

In fact we can see the type signatures of some built-in polymorphic
functions:

``` haskell
ghci> :t head
head :: [a] -> a
ghci> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Not sure what the type parameters are? Or, want to make your type
parameters explicit? We can use `forall`{.haskell} to introduce a
polymorphic function type, with the variables succeeding
`forall`{.haskell} being the type parameters to the function.

``` haskell
ghci> :set -fprint-explicit-foralls
ghci> :t head
head :: forall a. [a] -> a
ghci> :t (.)
(.) :: forall b c a. (b -> c) -> (a -> b) -> a -> c
ghci> :{
ghci| singleton :: forall a. a -> [a]
ghci| singleton x = [x]
ghci| :}
ghci> singleton 2
[2]
ghci> singleton 'a'
"a"
```

Let's inspect the type signature of `(.)`{.haskell}. Recall that this
function performs function composition; the implementation of
`(.)`{.haskell} might look something like this:

``` haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = g (f x)
```

We have three terms, `g`, `f` and `x`. We know that `g` and `f` must be
functions since we are calling them, thus we are going to let the types
of `g` and `f` to be `d -> c`{.haskell} and `a -> b`{.haskell}
respectively. Additionally, `x` is just some other term, and we will let
its type be `e`. Thus for now, we shall let the type signature of
`(.)`{.haskell} be the following, assuming the return type ultimately
becomes `r`:

``` haskell
(.) :: (d -> c) -> (a -> b) -> e -> r
(.) g f x = g (f x)
```

Now notice the following: for `f x`{.haskell} to be well-typed, the type
of `x` must be the same as the type of the parameter to `f`, which is
`a`. Thus, more accurately, `x` must be of type `a`:

``` haskell
(.) :: (d -> c) -> (a -> b) -> a -> r
(.) g f x = g (f x)
```

We can now see that `f x` is well-typed, and this expression is of type
`b`. We then pass this result into `g`. For this to be well-typed,
again, the parameter type of `g` must match the type of `f x`. Thus, `g`
must actually be of type `b -> c`{.haskell}:

``` haskell
(.) :: (b -> c) -> (a -> b) -> a -> r
(.) g f x = g (f x)
```

Finally, `g (f x)` has type `c`, which is what is returned from the
function. As such, the return type of `(.) g f x`{.haskell} should also
be `c`. This recovers the correct type signature shown by GHCI.

::: remark
You might be surprised to know that the process of recovering or
reconstructing the types is known as type inference, which as stated in
earlier chapters, is also done by GHC! When you omit the type signature
of any binding, GHC goes through this same process and helps us
determine what the type of that binding is.
:::

### Programming with Polymorphic Types/Functions

When should we define polymorphic types or functions? As we have shown,
when the implementations of classes, data types, functions etc. are the
same except for the types, then we are able to parameterize the
differing types which makes the class/data type/function polymorphic!
Knowing immediately when to create polymorphic types/functions takes
some practice, so to start, just create specialized versions of those
types/functions, and as the need arises, make them polymorphic by
parameterizing the appropriate types.

::: example
[]{#eg:polymorphictypetree label="eg:polymorphictypetree"} Suppose we
are trying to create a `Tree` class that represents binary trees. Should
this class be polymorphic? For now, let's ignore this fact and proceed
to create a naive implementation of this class. Further suppose we are
expecting to create a tree of integers, so we shall let that be the type
of the values of our tree.

``` python
@dataclass
class IntTree:
    pass
@dataclass
class IntNode(IntTree):
    left: IntTree
    value: int
    right: IntTree
@dataclass
class IntLeaf(IntTree):
    value: int
```

Looks great! From this class we are able to create binary trees of
integers, for example, `IntNode(IntLeaf(1), 2, IntLeaf(3))`{.python}
gives a binary tree with preorder 2, 1 and 3.

Further suppose later on we need to store strings in a binary tree.
Again, let's naively implement a separate class that does so:

``` python
@dataclass
class StrTree:
    pass
@dataclass
class StrNode(StrTree):
    left: StrTree
    value: str
    right: StrTree
@dataclass
class StrLeaf(StrTree):
    value: str
```

Once again, notice that the implementations of the classes are
identical, and the only difference is in the types! This is one clear
example where we should make our class polymorphic!

``` python
@dataclass
class Tree[a]:
    pass
@dataclass
class Node[a](Tree[a]):
    left: Tree[a]
    value: a
    right: Tree[a]
@dataclass
class Leaf[a](Tree[a]):
    value: a
```

Now from this one class, we are able to create all kinds of trees!
:::

::: example
[]{#eg:polymorphicfunctionreverse label="eg:polymorphicfunctionreverse"}
Suppose we are trying to define a function that reverses a list. Once
again, we have to be specific with the type of this function.
Temporarily, we shall create a function that works on lists of integers:

``` python
def reverse_int(ls: list[int]) -> list[int]:
    return [] if not ls else \
           reverse_int(ls[1:]) + [ls[0]]
```

Then, later on we might have to define a similar function that reverses
lists of strings:

``` python
def reverse_str(ls: list[str]) -> list[str]:
    return [] if not ls else \
           reverse_str(ls[1:]) + [ls[0]]
```

Once again, we can see that the implementations of the two functions are
identical, and only the types are different. Make this function
polymorphic!

``` python
def reverse[a](ls: list[a]) -> list[a]:
    return [] if not ls else \
           reverse(ls[1:]) + [ls[0]]
```
:::

[\[eg:polymorphictypetree\]](#eg:polymorphictypetree){reference-type="autoref"
reference="eg:polymorphictypetree"} and
[\[eg:polymorphicfunctionreverse\]](#eg:polymorphicfunctionreverse){reference-type="autoref"
reference="eg:polymorphicfunctionreverse"} give us some scenarios where
we discover that we have to make a class or function polymorphic. More
importantly, we see that that the implementations across the specialized
versions of the class/function are equal, and only the types differ. One
key insight we can draw from this is: a class/function should be made
polymorphic if its implementation is *independent* of the type(s) it is
representing/acting on.