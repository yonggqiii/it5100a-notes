![Updated][update-shield]

# First-Class Functions

When we say that a language has first-class functions, what we mean is that functions are just regular terms or objects just like other terms and objects that you frequently encounter. Therefore, they can be _assigned to variables_, _passed in as arguments_ and _returned from functions_. A language like Python (and of course, functional programming languages like Haskell and Lean) has first-class functions, making the following program completely valid:

```python
def foo():
    return 1
x = foo
y = x() # 1
```

Although this program seems extremely weird, especially for those who are familiar with languages like C and Java, it totally works. The idea is, at least in Python, that functions are also _objects_, and therefore the `foo` _name_ or _variable_ actually stores a reference to the function that always returns `1`. This reference can be assigned to any other variable like `x` because `foo` is also a reference to an object! Then, when we invoke `x`, the Python runtime looks-up the reference stored in `x` which points to the `foo` function, and thus evaluates to `1`.

Then, a function that receives functions as arguments or returns functions is known as a _higher-order function_. Let us look at the following examples:

```python
def add(x):
    def add_x(y):
        return x + y
    return add_x
```

Invoking this function is slightly weird, although still behaves more-or-less as expected:
```python
>>> add(1)(2)
3
```
As you can see, `add` defines a local function `add_x` that receives `y` and returns `x + y`, for whatever `x` was passed into `add`. Then, `add` returns the `add_x` function itself! Therefore, `add(1)` actually evaluates to the _function_ `add_x` where `x` is `1`, and when _that_ is invoked, it evaluates to `1 + 2` which is `3`! This is an example of a function that _returns_ a function, making it a higher-order function.

Another example is as follows:
```python
def map(f, it):
    return (f(i) for i in it)
```
This function _invokes_ the argument `f`, passing each `i` from `it`. Therefore, `f` is a function! An example of using `map` is as follows:
```python
>>> def add_1(x): return x + 1
>>> list(map(add_1, [1, 2, 3]))
[2, 3, 4]
```
As you can see, `map` applies `add_1` to every single element of `[1, 2, 3]` and yields them into the resulting list, thereby giving us `[2, 3, 4]`! Again, since `map` _receives_ functions like `add_1`, it is also a higher-order function.

Having to write simple functions like `add_1` is incredibly cumbersome. As such, languages like Python and Java make it easy to define _anonymous functions_, usually named _lambda expressions_[^1]. A lambda expression in Python looks like this:

```python
>>> list(map(lambda x: x + 1, [1, 2, 3]))
[2, 3, 4]
```
The idea is simple: the variable names to the left of `:` are the function's parameters, and the expression to the right of `:` is its return value. Obviously, this makes `lambda` expressions more restrictive since we cannot express multi-statement functions, but that is not the point. It provides a convenient syntax for defining short functions, which comes in handy very frequently.

## Nested Functions and Closures

You have likely been introduced to the idea of a _nested function_, i.e. it is a function that is defined locally within another function. And example is as follows:
```python
def f(x):
    def g(y):
        return x + y
    return g
```
`f` defines a nested local function `g`. In a sense, a nested function is just a function defined within a function. However, recall that local variables and definitions are typically erased from the _call stack_ once the function has returned. Therefore, when an expression like `f(2)` is evaluated, the Python runtime should allocate a stack frame for `f`, which, internally defines `g` and has the local binding for `x = 2`. The function returns the reference stored in `g`. As the function is returned, all the local variables should have been torn down, such as the local variable `g` (however, the heap reference stored in `g` (which points to the local function definition) is returned to the caller of `f`, so it remains in memory and is accessible). However, `x`, containing the reference to the value `2` should also be cleaned up since `x` is a local variable! In this case, how does `f(2)(3)` become `5` if the local variable `x` has been cleaned and the binding has been forgotten?

It turns out that languages that have first-class functions frequently support _closures_, that is, an environment that _remembers_ the bindings of local variables. Therefore, when `f(2)` is invoked, it does not return `g` as-is, with a reference to some local `x` with no binding. Instead, it returns `g` with an environment containing the binding `x = 2`. As such, when we then invoke _that_ function passing in `3` (i.e. `f(2)(3)`), it returns `x + y` where `y` is obviously `3`, but is also able to look up the environment `x = 2`, thereby evaluating to `5`.

### Currying
Nested functions and closures thereby support the phenomenon known as _currying_, which is to have a multi-parameter function being converted to successive single-parameter functions. Without loss of generality, suppose we have a function `f(x, y, z)`. _Currying_ this function gives us a function `f(x)`, which returns a function `g`, defined as `g(y)`, that function returns another function `h` defined as `h(z)`, and `h` does whatever computation `f(x, y, z)` does. We offer the following simple example:

```python
def add(x, y, z):
    return x + y + z

# Curried
def add_(x):
    def g(y):
        def h(z):
            return x + y + z
        return h
    return g

# Simpler definition with lambda expressions
def add__(x):
    return lambda y: lambda z: x + y + z
    # the scope of lambda expressions extend as far to the right
    # as possible, and therefore should be read as
    # lambda y: (lambda z: (x + y + z))
```
Currying supports _partial function application_, which supports code re-use. You will see _many_ instances of currying used throughout these notes, and hopefully this will become second-nature to you.

## Parameterizing Behaviour
Consider the following functions:
```python
def sum_naturals(n):
    return sum(i for i in range(1, n + 1))
def sum_cubes(n):
    return sum(i ** 3 for i in range(1, n + 1))
```
Clearly, the only difference between these two functions are the terms to sum. However, the difference in `i` and `i ** 3` cannot be abstracted into a single term. Instead, what we have to do is to abstract these as a function `f` on `i`! As such, what we want is to have a function that _parameterizes behaviour_, instead of just parameterizing values.

Since Python supports first-class functions, doing so is straightforward.

```python
def sum_terms(n, f):
    return sum(f(i) for i in range(1, n + 1))
```
Then, we can use our newly defined `sum_terms` function to re-define `sum_naturals` and `sum_cubes` easily:

```python
sum_naturals = lambda n: sum_terms(n, lambda i: i)
sum_cubes = lambda n: sum_terms(n, lambda i: i ** 3)
```

The process of abstracting over behaviour is no different when defining functions to abstract over data/values. Just _retain similarities and parameterize differences_! As another example, suppose we have two functions:
```python
def scale(n, seq):
    return (i * n for i in seq)
def square(seq):
    return (i ** 2 for i in seq)
```
Again, we can retain the similarities (most of the code is similar), and parameterize the behaviour of either scaling each `i` or squaring each `i`. This can be written as a function `transform`, which we can use to re-define `scale` and `square`:

```python
# If you notice carefully, this is more-or-less the implementation of map
def transform(f, seq):
    return (f(i) for i in seq)
scale = lambda n, s: transform(lambda i: i * n, s)
square = lambda s: transform(lambda i: i ** 2, s)
```
In fact, we can use the `transform` function to transform any iterable in whatever way we want!

## Manipulating Functions
On top of partial function application and parameterizing behaviour, we can use functions to manipulate/transform functions! Doing so typically requires us to define functions that _receive_ and _return_ functions. For example, if we want to create a function that receives a function `f` and returns a new function that applies `f` twice, we can write:
```python
def twice(f):
    return lambda x: f(f(x))

mult_four = twice(lambda x: x * 2)

print(mult_four(3)) # 12
```
As you can see, `twice` receives a function and returns a new function that applies the input function twice. In fact, we can take this further by generalizing `twice`, i.e. defining a function `compose` that performs function composition:
\\[(g\circ f)(x) = g(f(x))\\]
```python
def compose(g, f):
    return lambda x: g(f(x))

mult_four = compose(lambda x: x * 2, lambda x: x * 2)
plus_three_mult_two = compose(lambda x: x * 2, lambda x: x + 3)

print(mult_four(3)) # 12
print(plus_three_mult_two(5)) # 16
```

This is a really powerful idea and you will see this phenomenon frequently in this course. 

Specific to Python, we can use single-parameter function-manipulating functions like `twice` as decorators:
```python
@twice
def mult_four(x):
    return x * 2

print(mult_four(3)) # 12
```
Although the definition of `mult_four` actually only multiplies the argument by `2`, the `twice` decorator transforms it to be applied twice, therefore multiplying the argument by `4`! While decorators are useful, Haskell does not have decorators similar to this, although, frankly, this is not a required feature in Haskell since it has features much more ergonomic than this.

## Map, Filter, Reduce and FlatMap
There are several higher-order functions that are frequently used in programming. One of these functions is `map`, and is more-or-less defined as such:
```python
def map(f, ls):
    return (f(i) for i in ls)
```
This is exactly what you've seen earlier in `transform`! The idea is that `map` receives a function that maps each element of the iterable `ls`, and produces an iterable containing those transformed elements. Using it is incredibly straightforward:
```python
>>> list(map(lambda i: i + 1, [1, 2, 3]))
[2, 3, 4]
>>> list(map(lambda i: i * 2, [1, 2, 3]))
[2, 4, 6]
```
As you can see, `map` allows us to transform every element of an input iterable using a function. Another function, `filter`, filters out elements that do not meet a _predicate_:
```python
def filter(f, ls):
    return (i for i in ls if f(i))
```
```python
>>> list(filter(lambda x: x >= 0, [-2, -1, 0, 1]))
[0, 1]
```
`map` and `filter` are powerful tools for transforming an iterable/sequence. However, what about aggregations? For this, we have the `reduce` function:
```python
def reduce(f, it, init):
    for e in it:
        init = f(init, e)
    return init
```
As you can see, `reduce` receives three arguments: (1) a binary operation `f` that combines two elements (the left element is initially the `init` term, and also holds every successive application of `f`, i.e. it is the _accumulator_), (2) the iterable `it`, and (3) the initial value `init`. It essentially abstracts over the _accumulator_ pattern that you have frequently seen, such as a function that sums over numbers or reverses a list:
```python
def sum(ls):
    acc = 0
    for i in ls:
        acc = acc + i
    return acc

def reverse(ls):
    acc = []
    for i in ls:
        acc = [i] + acc
    return acc
```
In summary, `0` in `sum` and `[]` in `reverse` acts as `init` in `reduce`; `ls` in both functions act as `it` in `reduce`; `lambda acc, i: acc + i` and `lambda acc, i: [i] + acc` acts as `f` in `reduce`. We can therefore rewrite both of these functions using `reduce` as such:

```python
>>> sum = lambda ls: reduce(lambda x, y: x + y, ls, 0)
>>> reverse = lambda ls: reduce(lambda x, y: [y] + x, ls, [])
>>> sum([1, 2, 3, 4])
10
>>> reverse([1, 2, 3, 4])
[4, 3, 2, 1]
```
Another way to view `reduce` is as a _left-associative fold_. To give you an example, suppose we are calling `reduce` with arguments `f`, `[1, 2, 3, 4]` and `i` as the initial value. Then, `reduce(f, [1, 2, 3, 4], i)` would be equivalent to:

```
reduce(f, [1, 2, 3, 4], i) ==> f(f(f(f(i, 1), 2), 3), 4)
```

One last function that should be unfamiliar to Python developers is a `flatMap` function, which performs `map`, but also does a one-layer flattening of the result. This function is available in other languages like Java, JavaScript and many other languages due to its connection to _monads_, but we shall give a quick view of what it might look like in Python:
```python
def flat_map(f, it):
    for i in it:
        for j in f(i):
            yield j
```
The idea is that `f` receives an element of `it` and returns an _iterable_, and we loop through the elements of that iterable and yield them individually. Take for example a function that turns integers into lists of their digits:
```python
>>> to_digits = lambda n: list(map(int, str(n)))
>>> to_digits(1, 2, 3, 4)
[1, 2, 3, 4]
```
If we had used `map` over a list of integers, we get a two-dimensional list of integers, where each component list is the list of digits of the corresponding integer:
```python
>>> list(map(to_digits, [11, 22, 33]))
[[1, 1], [2, 2], [3, 3]]
```
If we had used `flat_map` instead, we would get the same mapping of integers into lists of digits; however, the list is flattened into a list of digits of all the integers:

```python
>>> list(flat_map(to_digits, [11, 22, 33]))
[1, 1, 2, 2, 3, 3]
```


---
[^1]: The term _lambda expression_ is inspired from the \\(\lambda\\)-calculus.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
