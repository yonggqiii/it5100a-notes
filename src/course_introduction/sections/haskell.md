![Updated][update-shield]
# Haskell

Haskell is a *statically-typed*, *purely functional*
*nonstrict-evaluation* programming language. Informally, static typing
means that we can look at a program (without executing it) and tell what
the type of any term is. A purely-functional language is a
language that supports only functional programming concepts (unlike
multi-paradigm languages like Python). Nonstrict-evaluation means that
there is no strict sequence of evaluating statements or expressions, and compilers are free
to decide which expressions should be evaluated first&mdash;*lazy
evaluation* is where expressions are evaluated only when they are needed. 
We will look at non-strict evaluation eventually; for now, understanding static typing and purely
functional programming is more important.

In a purely functional language like Haskell, you will miss the
following programming language features that are present in virtually
every general-purpose programming language:

-   Mutation (even variables are immutable);

-   Loops;

-   Objects (classes etc.);

-   Dynamic typing (e.g. `x` can be an `int` now, and
    a `str` later);

You might find it difficult to adjust to such a programming environment.
However, you will find these restrictions meaningful as we have alluded
to in the previous section.

## Basic Expressions

By this point you should have already installed GHC, which comes with
two main parts: `ghc` itself (the compiler), and `ghci` the
REPL/interpreter. For now, run `ghci` in the terminal to start an interactive Haskell
shell, and enter some basic mathematical expressions!

``` haskell
ghci> 1 + 2 - 3
0
ghci> 1 * 2 / 4
0.5
ghci> 5 ^ 2 `mod` 5
0
ghci> 5 `div` 2
2
```

Note some differences: `^` is exponentiation (just as you would normally
type in a calculator), and there is no modulo operator. There is a
modulo function called `mod`, and you can apply any binary
function in an *infix* manner by surrounding the function in backticks. Integer
division is a function `div`. The operator precedence rules apply.

In a functional programming language like Haskell, it should come as no
surprise that virtually everything is a function. Mathematical operators
are actually just functions! In GHCI, we can observe the type of any
term (terms are sort of like objects in Python; functions are terms!) using `:t`, and we can show the type of
the function of the `+` operator by issuing `:t (+)` (when writing
operators as a term in the usual prefix notation, surround it in parentheses).
We can in fact re-write an infix operator function call as a normal prefix
function call. Note that in Haskell, `f x y z` is essentially
the same as `f(x, y, z)` in languages like Python.

``` haskell
ghci> :t (+)
Num a => a -> a -> a
ghci> 2 + 3
5
ghci> (+) 2 3
5
```

As we know, currying is the act of translating an \\(n\\)-ary function to a
unary function that receives one parameter and returns a function that
receives the remaining parameters (in curried form). In Haskell, all
functions are curried, so even a function like `(+)` really
looks something like this in Python:

``` python
def add(x):
    return lambda y: x + y
```

This is automatically done in Haskell. Thus we might be able to write
our Python equivalent of `add(2)` directly in Haskell as
`(+2)`:

``` haskell
ghci> y = (+2)              
ghci> y 3
5
```

which in Python, looks like:

``` python
>>> def add(x): return lambda y: x + y
>>> y = add(2)
>>> y(3)
5
```

Therefore, to be more specific, `f x y z` in Haskell is more
like `f(x)(y)(z)` in Python.

We can also load Haskell source files into GHCI. Python source files
have the `.py` extension; Haskell source files instead have the `.hs`
extension. Let us try writing a simple Haskell program. Create a new
file like `MyCode.hs` and write in the following:

``` haskell
-- MyCode.hs
main :: IO () -- entry point to the program
main = putStrLn "Hello World!"
```

We will look at what the first line means in the future. For now, try
compiling and running your code by issuing the following commands in
your terminal (windows users might have to run `./MyCode.exe`):

```
ghc MyCode.hs
./MyCode
```
The first command invokes GHC to *compile* your source file.
*Compilation* translates your source file into an *executable* file that
your computer that understand. The compilation process will also perform
a bunch of compile-time checks, such as type-checking etc. It may also
perform some optimizations. The outcome of invoking that command is an
executable (probably called `MyCode`) along with other files (which we
shall not talk about for now). The second command then executes that
executable, and you should see `Hello World!` shown in the terminal.

```output info
Hello World!
```

We shall ignore compiling source files for now and temporarily focus on
working with GHCI. In GHCI, we can load files by issuing `:l MyFile.hs`,
which loads the source into the shell. For now, write the following code
in `MyCode.hs`:

``` haskell
-- MyCode.hs
z = 1 -- ok
y = 2 -- ok
y = 3 -- not ok!
```

As we have described earlier, everything in Haskell is immutable.
Therefore, re-defining what `y` is should be disallowed! Let's try
loading `MyCode.hs` into GHCI:

```output error
ghci> :l MyCode.hs
[1 of 2] Compiling Main ( MyCode.hs, interpreted )

MyCode.hs:4:1: error:
    Multiple declarations of 'y'
    Declared at: MyCode.hs:3:1
                 MyCode.hs:4:1
  |
4 | y = 3 -- not ok!
  | ^
```

As you can see, you cannot redefine functions or variables. Everything
is immutable in Haskell! Therefore, the statement `x = e` is **not** 
an assignment statement. Rather, it is a _bind_ or a _definition_.

## Control Structures

In Haskell, you mainly write *expressions*, and not statements.
Consequently, there are only `if`-`else` expressions, and no `if`-`else`
statements. That means that you cannot omit an `else` branch of an
`if`-`else` expression, just like in Python:

``` python
>>> x = 2 * -1
>>> y = 'positive' if x == 2 else 'negative'
>>> y
'negative'
```

In Haskell, this would be (negative numbers must be surrounded by parentheses, otherwise Haskell thinks it is a partial function application of subtraction `(-)`):

``` haskell
ghci> x = 2 * (-1)
ghci> y = if x == 2 then "positive" else "negative"
ghci> y
"negative"
```

Just like in Python, `if`-`then`-`else` expressions in Haskell are *expressions* and therefore
evaluate to a term:

``` haskell
ghci> (if 1 /= 2 then 3 else 4) + 5
8
```

Note that *not equals* looks like `/=` in Haskell but `!=` in Python.
The equivalent expression in Python might be:

``` python
>>> (3 if 1 != 2 else 4) + 5
8
```

Importantly, the type of any expression is fixed, or at least, we should
be able to determine what the type of every expression is unambiguously
just by looking at it. Therefore, writing the following expression in
Haskell will throw an error:

``` haskell
ghci> x = 2 * (-1)
ghci> y = if x == 2 then 2 else "negative"
<interactive>:2:20: error:
  - No instance for (Num String) arising from the literal '2'
  - In the expression: 2
    In the expression: if x == 2 then 2 else "negative"
    In an equation for 'y': y = if x == 2 then 2 else "negative"
```

The reason is that we should not need to evaluate the truth of
`x == 2` to determine what the type of the entire `if`-`else`
expression is. Thus, Haskell requires that the type of the expression in
the `if` branch be the same as the type of the expression in the `else`
branch. This departs from Python which is *dynamically typed*, where
types are determined at runtime, so expressions can freely be of
different types based on the values they inherit at the time of program
execution.

## Functions

Defining functions in Haskell looks like defining a variable. This
should be expected since Haskell is centred around functions, so it
should come as no surprise that functions do not need to be defined with
any special syntax.

``` haskell
ghci> oddOrEven x = if even x then "even" else "odd"
ghci> oddOrEven 1
"odd"
ghci> oddOrEven 2
"even"

ghci> quadratic c2 c1 c0 x = c2 * x ^ 2 + c1 * x + c0
ghci> f = quadratic 1 2 3 -- x^2 + 2x + 3
ghci> f 4
27
ghci> f 5
38
```

We might then ask: how do we write a loop in Haskell? Like we said
earlier, Haskell is a purely functional programming language, so there
are no loops (we may later see loops being simulated with functions).
Thus, for now we shall use recursion as it is often the most elegant way
to solve problems.

Recall that the familiar `factorial` function may be written imperatively
in Python as:

``` python
def fac(n):
    res = 1
    for i in range(2, n + 1):
        res *= i
    return res
```

As we know, the factorial function can be defined recursively as such:
$$n! = \begin{cases}
  1 & \text{if }n=0\\\\
  n \times (n - 1)!& \text{otherwise}
\end{cases}$$ And in Python:

``` python
def fac(n):
    return 1 if n == 0 else \
           n * fac(n - 1)
```

In Haskell, we are free to do the same:

``` haskell
ghci> fac n = if n == 0 then 1 else n * fac (n - 1)
ghci> fac 4
24
```

In fact, we can also express functions like this elegantly in Haskell
with *guards*. Guards allow us to define expressions differently based
on a condition.

For example, we know that the Fibonacci function may be written like so:
$$\textit{fib}(n) = \begin{cases}
            1 & \text{if } n = 0\\\\
              1 & \text{if }n = 1\\\\
              \textit{fib}(n - 1) + \textit{fib}(n - 2) & \text{otherwise}
            \end{cases}$$ 

And writing this function with regular `if`-`else`
expressions might look like: [^5]

``` haskell
ghci> :{
ghci| fib n = if n == 0 || n == 1 
ghci|         then 1 
ghci|         else fib (n - 1) + fib (n - 2)
ghci| :}
```

However, it might look clearer to define it this way with guards (`otherwise` is just defined as `True`):

``` haskell
ghci> :{
ghci| fib n
ghci|   | n == 0    = 1
ghci|   | n == 1    = 1
ghci|   | otherwise = fib (n - 1) + fib (n - 2)
ghci| :}
ghci> fib 5
8
```

Even better, we can use *pattern matching* to define such functions much
more easily. We will look at pattern matching in more detail in the
future:

``` haskell
ghci> fib 0 = 1
ghci> fib 1 = 1
ghci> fib n = fib (n - 1) + fib (n - 2)
ghci> fib 5
8
```

## Auxiliary Bindings
Thus far we have defined functions as a single expression; this is akin
to writing a *lambda expression* in Python. As we know, that may not
always be the most ergonomic considering that many functions can be
better defined with several 'statements' that lead into a final
expression. One example would be the following in Python:

``` python
def weight_sum(n1, w1, n2, w2):
    x = n1 * w1
    y = n2 * w2
    return x + y
```

While it is completely acceptable to define this function in one line,
it is not as readable. In Haskell, functions indeed have to be written
as a single expression, but we can define local bindings for the
expression using `let`:

``` haskell
ghci> :{
ghci| weightSum n1 w1 n2 w2 =
ghci|   let x = n1 * w1
ghci|       y = n2 * w2
ghci|   in  x + y
ghci| :}
ghci> weightSum 2 3 4 5
26
```

The `let` binding allows us to introduce the definitions of
`x` and `y` which are used in the expression after
the `in` clause. These make writing larger expressions more readable.

`let` bindings are (more-or-less) *syntax sugar* for function calls:

``` haskell
weightSum n1 w1 n2 w2 = 
    let x = n1 * w1
        y = n2 * w2
    in  x + y
 
-- same as
 
weightSum n1 w1 n2 w2 =
    f (n1 * w1) (n2 * w2)

f x y = x + y
```

Importantly, `let` bindings are expressions; they therefore evaluate to
a value, as seen in this example:

``` haskell
ghci> (let x = 1 + 2 in x * 3) + 4
13
```

This is different to `where` bindings, which also allow us to write
auxiliary definitions that support the main definition:

``` haskell
weightSum n1 w1 n2 w2 = 
    let x = n1 * w1
        y = n2 * w2
    in  x + y
 
-- same as

weightSum n1 w1 n2 w2 = x + y
    where x = n1 * w1
          y = n2 * w2
```

Other differences between `let` and `where` are not so apparent at this
stage. You are free to use either appropriately (use `let` where an
expression is desired, using either `let` or `where` are both okay in
other scenarios).

## Data Types

We have looked at some simple data types so far: numbers like
`1.2`, and strings like `"abc"`. Strings are
actually **lists** of characters! Strings are surrounded by double
quotes, and characters are surrounded by single quotes, like
`'a'`.

Lists in Haskell are *singly-linked list* with homogenous data. That
means that the types of the elements in the list must be the same. We
can write lists using very familiar syntax, e.g. `[1, 2, 3]`
being a list containing the numbers 1, 2 and 3. Indexing a list can be
done with the `!!` function.

``` haskell
ghci> x = [1, 2, 3]
ghci> x !! 1 -- indexing, like x[1]
2
```

We can also construct ranges of numbers, or any enumerable type (such as
characters). The syntax for creating such lists is straightforward as
shown in the examples below.

``` haskell
ghci> y = [1,3..7] -- list(range(1, 8, 2))
ghci> y
[1,3,5,7]
ghci> z = [1..10]  -- list(range(1, 11))
ghci> z
[1,2,3,4,5,6,7,8,9,10]
ghci> inflist = [1..] -- 1,2,3,...
ghci> inflist !! 10
11
```

As we stated earlier, strings are lists of characters, we can even build
ranges of characters which result in strings.

``` haskell
ghci> ['h', 'e', 'l', 'l', 'o']
"hello"
ghci> ['a'..'e']
"abcde"
ghci> ['a'..'e'] ++ ['A'..'D'] -- ++ is concatentation
"abcdeABCD"
```

As you know, a singly-linked list is one of two things: an empty list,
or a node with a value (`head`) and a reference to the remaining part of
the list (`tail`). Thus, one of the most frequently used operations is the
*cons* operation (`:`) which builds (or de-structures) a list
given its head and tail values. The `:` operator is
right-associative.

``` haskell
ghci> x = [1, 2, 3]
ghci> 0 : x
[0,1,2,3]
ghci> 0 : 1 : 2 : 3 : []
[0,1,2,3]
ghci> 'a' : "bcde"
"abcde"
```

One of the most interesting parts of Haskell is that it has
non-strict evaluation. That means that the compiler is free to evaluate
any expression only when it is needed. This allows us to quite nicely
define recursive data without running into infinite loops:

``` haskell
ghci> y = 1 : y
ghci> take 5 y
[1,1,1,1,1]
```

As we know, performing recursion over a list frequently requires us to
get a head element and then recursively calling the function over the
remaining list. This is nicely supported without any performance costs
unlike in Python, where `ls[1:]` runs in \\(O(n)\\). For example,
writing a function that sums a list of numbers might look like the
following in Python:

``` python
def sum(ls):
    if len(ls) == 0:
        return 0
    return ls[0] + sum(ls[1:])
```

Haskell is very similar (`head` is a function that returns the
first element of a list, and `tail` is a function that returns
the remainder of a list):

``` haskell
sum' ls = if length ls == 0
          then 0
          else head ls + sum' (tail ls)
```

As a quick aside, the `:` operator is really a _constructor_ for
lists, so in fact we can use pattern matching (again, we will discuss
this in the future) to define the `sum'` function very
elegantly.

``` haskell
sum' [] = 0
sum' (x : xs) = x + sum' xs
```

Python also supports *list comprehension* as you may recall:

``` python
>>> x = [1, 2, 3]
>>> y = 'abc'
>>> [(i, j) for i in x for j in y if i % 2 == 1]
[(1, 'a'), (1, 'b'), (1, 'c'), (3, 'a'), (3, 'b'), (3, 'c')]
```

Haskell also provides the same facility, with different syntax:

``` haskell
ghci> x = [1, 2, 3]
ghci> y = "abc"
ghci> [(i, j) | i <- x, j <- y, odd i]
[(1,'a'),(1,'b'),(1,'c'),(3,'a'),(3,'b'),(3,'c')]
```

At this junction it would be most appropriate to discuss tuples. Like
Python, the fields of a tuple can be of different types. However, tuples
in Haskell are **not** sequences. Tuples behave more like the product of
several types, as is usually the case in many domains.

As such, there are not many operations we can do on tuples. One of the
only special cases is pairs, which have functions to project each value:

``` haskell
ghci> fst (1,"abc")
1
ghci> snd (1,(2,[3,4,5]))
(2,[3,4,5])
ghci> snd (snd (1,(2,[3,4,5])))
[3,4,5]
```

This should suffice for now. Now is your turn to try the exercises to
get you started on your functional programming journey! Note that many
of the functions we have used are built-in to Haskell, as defined in
[Haskell's Prelude library](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html). You may want to refer to this library when doing the exercises. A large
portion of the Prelude documentation may be unreadable at this point, however, rest
assured that many of the concepts presented in the documentation will
be covered in this course.

[^5]: Note that `:{` and `:}` are used only in GHCI to define blocks of
    code, and are not part of Haskell.


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20SEP%202024-57ffd8?style=for-the-badge
