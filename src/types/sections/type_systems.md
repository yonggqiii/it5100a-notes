![Updated][update-shield]

# Type Systems
As the course title suggests, Haskell is a typed functional programming
language&mdash;in particular, it uses a statically-typed *type system*. This
begs the question, "what is a type system?"

An online search for definitions might give you the following:

> **Definition (Type System)**. A type system is a
**tractable syntactic method** for **proving the absence of certain
program behaviours** by classifying phrases according to the **kinds of
values they compute**.

Let us unpack the highlighted phrases in the definition above.

#### Tractable syntactic method

*Tractable* more or less means *easy*, or *polynomial time*. *Method*
refers to a *formal method*, which means it is a kind of mathematically
formal process. The fact that it is a *syntactic method* means that this
formal analysis can be done syntactically, without the need to appeal to
a *semantic* analysis (although, static type checking is done against
the static semantics of the type system). More or less, it can be
performed without executing any of the code it is analyzing.

#### Proving the absence of certain program behaviours

In the case of type systems, this usually means that the type system is
used to prove the absence of type errors. The realm of program analysis
is broken down into roughly two kinds: over-approximation analyses, and
under-approximation analyses. Notice that both perform *approximations*
of program behaviour&mdash;this is because obtaining a precise specification of
any program is *undecidable*. Generally, static analyses, like type
checking, perform an over-approximation of program behaviour. An analogy
of how this works is as follows: assume true program behaviour is \\(x\\)
and buggy behaviour is at \\(y\\) (these are all positive numbers, let's
say). We then over-approximate the true program behaviour, giving us
\\(x + \epsilon\\). If we can show that \\(x + \epsilon < y\\), then we can
guarantee that \\(x < y\\), so the program is not buggy.

A more concrete example is as follows. Let's suppose we have the following code snippet in Python:

``` python
y: int = 0 if f() else 'abc'
print(y + 1)
```

Notice that if we can determine that `f` always returns
`True`, then we know for sure that there will be no type
errors. However, it is not possible to make this determination in
general. Thus, we over-approximate program behaviour by assuming that it
is possible that `f` may return either `True` or
`False`leading us to show that we cannot prove the absence of
type errors in this program. Instead, if we had written the following:

``` python
y: int = 0 if f() else 1
print(y + 1)
```

Then even by assuming that both branches of the conditional expression
may be the result, we can conclusively show that `y` will
always be an `int`. Our over-approximation of program behaviour
doesn't have type errors, meaning, that our actual program really does
not have type errors.

#### Kinds of values they compute

This is a simple description of what *types* are. Types, as we will
informally define later, are classifications of data/things in the
program that all behave similarly or have similar characteristics. In
some other sense, types can be seen as abstractions over terms.

Simply put, a type system is a formal system that lets us show that
there won't be type errors. As we have seen, the nature of
\[statically-typed\] type systems forces us to program in a different
way (at least compared to dynamically typed languages like Python), and
this is what we will explore in this chapter.

## Types

Type systems are systems of types; but *what is a type*? In essence, a
type is like a *kind* of thing, or a high-level description of what
something is. Types (1) give meaning to some data, and (2) describe what
its members are like.

Since you have already programmed in Python, you should have some
inkling of what types are. In Python, everything is an object. Thus, in
Python, the type of an object is the *class* from which it was
instantiated.

The following is some sample output showing the types of various objects. The output of
all these function calls are classes.

```python
>>> x = 1
>>> type(x)
<class 'int'>
>>> type('abc')
<class 'str'>
>>> class A: pass
>>> type(A())
<class '__main__.A'>
```

This is very apt&mdash;classes are blueprints for creating objects, and (for
the most part), all *instances* of a class will abide by the
specification as laid out in the class. Therefore, Python's type system
based on classes is very appropriate for our purposes. In fact, this is
not unique to Python. Many other languages with OO features also have
classes as types.

In Python, we mainly think of types as being bound to *objects*, that
is, objects have *reified* types that can be accessed at runtime. We
have never thought of assigning types to variables or function
parameters, since when we are investigating the type of a variable, what
we are really doing is investigating the type of the object that is
referred to by the variable. However, Python actually does allow us to
annotate variables, function parameters etc with types to document
"suggestions" as to what the types of the objects assigned to them
should be.

Observe the following program fragment.

``` python
def f(x: int) -> str:
    y: int = x * 2
    return f'{x} * 2 = {y}'
z: int
z = 3
s: str = f(z)
print(s) # 3 * 2 = 6
```

This program fragment contains several *type annotations*. In the
function header, we have a specification for `f` to receive an
`int` and return a `str`. That is, if the type
annotations make sense, then passing an `int` into `f`
will always result in a `str`. In the function body, we also
have an annotation for the variable `y` stating that it is also
an `int`. This makes sense&mdash;if `x` is an `int`,
then so will `x * 2`. Actually, the type of `y` can be
*inferred* (a type checker can determine the type of `y`
automatically), so our type annotation for it is not necessary. Outside
the function body we have other type annotations, documenting what the
types of the other variables are. On visual inspection, we can see that
all the type annotations make sense and we have adhered to them fully;
we are thus guaranteed that we have no type errors.

While Haskell also provides the capability for type annotations, a
notable distinction lies in Haskell's *enforcement* of adherence to
these annotations. Consequently, it might be more fitting to refer to
them as *type declarations*. Nevertheless, the core concept remains
unchanged: specifying the types of variables, functions, or terms
ensures that, when adhered to correctly, our program will be well-typed.

The following code snippet shows some Haskell code with type
declarations.

``` haskell
f :: Int -> String
f x = show x ++ " * 2 = " ++ show y
    where y = x * 2
z :: Int
z = 3
s :: String
s = f(z) -- 3 * 2 = 6
```

A natural question would be to ask, what types can we declare variables
to be of? We have looked at some basic types earlier, `Int`,
`String` (which is an alias for `[Char]`),
`Char`, `[Int]`, `Bool`,
`Double` etc. There are many other types in Haskell's Prelude,
and later on we will see how we can create our own types.

Declaring types for functions is slightly different. In Python, when
writing type annotations for functions, we are really annotating the
types of its parameters, and its return type. In Haskell, we are
declaring the type of the function itself. The difference is actually
not as large as one might imagine. If the function receives a type \\(S\\)
and returns a type \\(T\\), then the function has the type \\(S\to T\\). We
similarly use arrows to declare the type of functions in Haskell. Thus,
as above, since `f` receives an `Int` and returns a
`String`, then `f` itself is of the type
`Int -> String`.

Haskell has roots in formal systems, in particular, System \\(F_C\\), which
is a dialect of System \\(F\omega\\) (without type lambdas). Thus, the types
of terms can be described formally. Knowing the formal typing rules of
Haskell is not required, but may give you some insight as to how it
works. Below we show the typing rules for function declarations, more
accurately, lambda abstractions.

\\[\frac{\Gamma,x:S\vdash e: T}{\Gamma\vdash\lambda x.e : S \to T}\text{T-Abs}\\]

The T-Abs rule is an *inference rule* stating that if the
premise above the line is true, then the conclusion below the line will
also be true. Let's first parse the premise. The part to the left of
\\(\vdash\\) is the *typing environment*, more or less describing the type
declarations we have at the point of analysis of the program.
Specifically, \\(\Gamma\\) is the actual type environment, while \\(x: S\\) is
an additional assumption that a variable \\(x\\) has type \\(S\\). The part to
the right of \\(\vdash\\) describes the judgement of the type of \\(e\\) being
\\(T\\). Overall, the premise states \"given what we have so far, if in
assuming \\(x\\) is of type \\(S\\) we get that \\(e\\) is of type \\(T\\), \...\". The
conclusion can be understood similarly: it states that the typing
environment \\(\Gamma\\) will show that the function \\(\lambda x.e\\) has type
\\(S \to T\\). Putting these together, the rule states that \"given typing
environment \\(\Gamma\\), if by assuming that variable \\(x\\) has type \\(S\\) we
get that the expression \\(e\\) is of type \\(T\\), then \\(\Gamma\\) will also show
that the type of the function \\(\lambda x.e\\) is of type \\(S \to T\\)\".

A simple demonstration in Python is as follows: suppose we have \\(x\\) as
`x` and \\(e\\) as `x * 2`. If we assume that `x`
is of type `int`, then we know that `x * 2` will also
be an `int`. Therefore, the type of \\(\lambda x.e\\) which is
`lambda x: x * 2` is `int -> int`[^1].

What about multi-parameter functions? Remember that in Haskell, all
functions are curried, thus, all functions in Haskell are single
parameter functions. Curried functions receive one parameter, and return
a function *closure* that receives the remaining variables and
eventually will return the final result. Therefore the `(+)`
function actually looks more like:

``` python
# Python
def add(x):
    return lambda y: x + y
```

The type of `add` is more like `int -> (int -> int)`.
This is (more or less) the type of `(+)` in Haskell, which
(more or less) has type `Int -> Int -> Int`. Note that
`->` is right-associative, so `Int -> Int -> Int` is
the same as `Int -> (Int -> Int)`.

In Haskell, the types of everything are **fixed**. This should be
unsurprising since everything in Haskell is immutable, but it is a
restriction that can also be found in other less restrictive languages
like Java and C++. In this environment, we have to, perhaps ahead of
time, decide what the type of a variable, function, function parameter
is, then write the implementation of your function around those
restrictions.

The following code
snippet first *declares* the type of `f` before showing its
implementation. It is not only good practice to declare types above
their implementation, but it can be a nice way to frame your mind around
the implementation of your function&mdash;start by providing a high-level
specification of your function, then work on the implementation to
describe what the function is actually trying to achieve.

``` haskell
f :: Int -> String -- explicit type declaration
f x = show x ++ "!"
g x = x + 1 -- type of g is inferred
```

However, observe that the type of `g` is not defined. This
does not mean that the type of `g` is dynamic or is not being
checked; rather, Haskell can infer the *principal* (most liberal) type
of `g` via a process known as *type inference*. That still
means that the implementation of `g` itself must be well-typed
(its implementation does not break any of the typing rules), and that
any users of `g` must abide by its static type signature.

Generally speaking, it is good practice to declare the types of
top-level bindings&mdash;that is, nested bindings of functions, variables (for
example, in `let` expressions) do not need type declarations
and can often be inferred. The example above of the declaration of
`f` is a perfectly idiomatic way of defining and declaring a
function, unlike `g` which lacks a type declaration.

### Programming with Types

When learning Python, you might not have had to think very much about
types; this is because Python does not care about type annotations. For
example, you can happily annotate a variable to be an `int` but
then assign a string into it. This is very much unlike Haskell, where
adherence to type declarations and well-typedness is *enforced* by the
compiler&mdash;the compiler will reject any program that is not well-typed.

Observe the following program fragment:

``` haskell
f :: Int -> String -- explicit type declaration
f x = show x ++ "!"
 
g = f "1" -- compiler throws type error as f receives Int, not String
```

The definition of `f` is well-typed since it abides by all the
typing rules, and all the types make sense. However, since `f`
only receives `Int`, passing a `String` into it is a
clear violation of the rules. Thus, the entire program is ill-typed and
will not be compiled. Try this for yourself!

Programming in such a strict and formal language can feel restrictive,
but these restrictions actually feel more like "guard rails" or
"seatbelts"; if your program passes the checks done by the compiler, you
can be quite assured that it works. As the saying goes, in Haskell, \"if
it compiles, it works\". Although this is not necessarily true,
Haskell's robust and expressive type system allows you to rule out a
large class of bugs, and often, results in correct programs. However,
one question to ask is: how do we go about programming with static
types?

The first step of being able to program with types is understanding the
typing rules. We shall elide explanation on how typing works with
inference, typeclasses, polymorphism etc. and focus solely on the
simplest typing rules:

1.  In a binding `x = e`, the type of `x` must be
    the same as the type of `e`

2.  In a conditional expression `if x then y else z`, the type
    of `x` must be `Bool` and the types of
    `y` and `z` must both be equal to some type
    `a`; the type of the entire expression is `a`

3.  In a function application expression `f x` the type of
    `f` must be `a -> b` for some `a` and
    `b`, `x` must be of type `a`, and the
    type of the expression is `b`

4.  (Without loss of generality of number of parameters) For a function
    binding `f x = e` the type of `f` must be
    `a -> b` for some `a` and `b` and in
    assuming `x` to be of type `a`, `e`
    must be of type `b`.

Try calculating the types of every expression in the following code
snippet. Can you get it all right?

``` haskell
f :: Int -> Int -> [Int]
f x n =
  if n == 0 then
    []
  else
    let r = f x (n - 1)
    in  x : r
```

Let's work through this example. 
- We are declaring `f` to be of type `Int -> Int -> [Int]`, so it stands to reason that in the definition of `f` we are assuming that `x` and `n` are both of type `Int`. 
- For this to be well-typed, we must ensure that the conditional expression evaluates to `[Int]`, that means both branches must themselves evaluate to `[Int]`. 
- First we observe the condition `n == 0`; the `(==)` function receives two numbers and returns a `Bool`, so this is well-typed.
- Looking at the `True` branch, we see that we are returning the empty list, which matches the type of `[Int]`. 
- In the `False` branch, we have a `let` expression, so we must ensure that `x : r` evaluates to `[Int]` too. 
- The `let` binding contains a binding `r = f x (n - 1)`; knowing that (by our own declaration) `f` has type `Int -> Int -> [Int]`, knowing that `x` and `n - 1` are of type `Int` means we can safely conclude that `r` has type `[Int]` (of course, the `(-)` function receives two
integers and returns an integer). 
- The `(:)` function receives an `Int` and a `[Int]` and returns a
`[Int]`, so all the types match. 

Overall, we have seen that we
successfully determined the types of every expression in the program
fragment, and concluded that it is well-typed.

Now that you are familiar with the basic typing rules and (roughly) how
types are inferred, the next step is to get comfortable writing programs
with static types. Generally this comes with practice, but one great way
to get you started with typeful programming is to try letting the _types guide your programming_.

Suppose we are trying to define a function `f` that receives
an integer `x` and returns a string showing the result of
multiplying `x` by 2:

``` haskell
ghci> f 3
"3 * 2 = 6"
ghci> f 5
"5 * 2 = 10"
```

Let us try implementing this function. The first thing we have to
consider is the type of `f` itself, which by definition,
should receive an `Int` and return a `String`. As
such, we may start with the type declaration
`f :: Int -> String`.

Next, we know we are eventually going to have to convert `x`
into a `String`. We know that there is a `show`
function that does that. Its type signature (modified) is
`Int -> String`, so we know that `show x` is a
`String`.

We also know that we need to multiply `x` by 2. For this, we
can use the `(*)` function, which has a (modified) type
signature of `Int -> Int -> Int`. Thus, we can write
`x * 2` and that gives us an `Int`. Knowing that we
eventually need to display it as a `String`, once again, we
can rely on the `show` function.

Now we have all the numbers we need in `String` form, we need
to concatenate them together. For this, we can rely on our trusty
`(++)` function that receives two `String`s and
returns a `String`. Using this allows us to concatenate all
our desired strings together. Since our original function `f`
was meant to return a `String`, we can return it as our final
result.

``` haskell
f :: Int -> String
f x = 
  let sx :: String = show x
      y  :: Int    = x * 2
      sy :: String = show y
  in  sx ++ " * 2 = " ++ sy
```

This is a simple example of using
types to guide your programming. While seemingly trivial, this skill can
be incredibly useful for defining **recursive** functions!

Suppose we are trying to define a function that sums the integers in a
list. As always, we must decide what the type of this function is. As per our
definition, it receives a list of integers and returns the final sum,
which should be an integer as well. This gives us the type declaration
`sum' :: [Int] -> Int`.

First, let us define the base case. We should be quite clear on what the
condition for the base case is: it should be when the input list is empty.
What should we return in the base case? By our type declaration, we must
return an `Int`, so we must express our base result in that
type. The result is `0`, which matches our type declaration.

Next we must define the recursive case. This one might be tricky
initially. We know that we can make our recursive call, passing in the
tail of the input list. This might look something like
`sum' (tail ls)`. We must be very clear about the type of this
expression; as per the type declaration, the result is an
`Int`, and not anything else.

We also know that we want to add the head of the input list to the
result of the recursive call. In doing so we get an `Int`.

Finally, we can add the results together, giving us an `Int`,
which matches our return type.

``` haskell
sum' :: [Int] -> Int
sum' ls = 
    if null ls
    then 0
    else let r  :: Int = sum' (tail ls)
             hd :: Int = head ls
         in  hd + r
```

By getting used to types, having a statically-typed system no longer
feels like a chore or a hurdle to cross, and instead feels like a
support system that makes everything you are doing clear! Many
developers (including myself) love statically-typed programming
languages for this very reason, so much so that people have gone to
great lengths to add static typing to otherwise dynamically typed
languages like JavaScript (the typed variant of JavaScript is TypeScript).

Python is no different. Several static type checkers are out there to
help us analyze the well-typedness of our program. One of the most
popular analyzers is `mypy`, which was heavily developed by Dropbox.
However, I recommend `pyright` because at the time of writing, it has
implemented bleeding edge features that we need for further discussion
of types which we shall see very shortly.

Let's see `pyright` in action. We shall write an ill-typed
program and see if it catches the potential bug:

``` python
# main.py
def f(x: int, y: int) -> int:
    z = x / y
    return z
```
Running `pyright` on this program will reveal an error message:
```
pyright main.py
```

```output error
pyright main.py
/home/main.py
  /home/main.py:4:12 - error:
    Expression of type "float" is incompatible with return
    type "int"
    "float" is incompatible with "int" (reportReturnType)
1 error, 0 warnings, 0 informations 
``````
Great! This makes sense because assuming `x` and `y`
are of type `int`, the type of `z` should actually be
`float`! Let's correct the program and try running `pyright`
against the new program:

```python
# main.py
def f(x: int, y: int) -> int:
    z = x // y
    return z
```

```output info
pyright main.py
0 errors, 0 warnings, 0 informations
```
Very well! We have now learnt how to program with types in Haskell and
in Python, and since Python does not come with a type-checker, we are
able to use tools like `pyright` to do the type checking for us!

One additional great feature about `pyright` is that it is actually also a
language server. As such, you can include `pyright` in your favourite text
editors so that it can catch bugs while writing programs!

---

[^1]: Python doesn't have arrow types. The actual type of the function
    is `Callable[[int], int]`.


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
