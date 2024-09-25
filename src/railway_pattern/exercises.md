# Exercises

These exercises has questions that will require you to write code in Python and Haskell. All your Python code should be written in a purely-functional style.

## Question 1

Create the following ADTs in Python:
- A singly linked list
- A `Maybe`-like type, with "constructors" `Just` and `Nothing`
- An `Either`-like type, with "constructors" `Left` and `Right`
- A `Validation`-like type, with "constructors" `Success` and `Failure`. Because Python does not have higher-kinds, you may assume that `Failure`s always hold a list of strings.

Then define methods on all these types so that they are all functors, applicatives and monads (`Validation` does not need to be a monad). `fmap` can be called `map`, `<*>` can be called `ap`, `return` can just be `pure`, and `>>=` can be called `flatMap`.

Due to Python's inexpressive type system, you are free to omit type annotations.

**Try not to look at Haskell's definitions when doing this exercise to truly understand how these data structures work!**

Example runs for each data structure follow:

#### Lists
```python
# lists
>>> my_list = Node(1, Node(2, Empty()))

# map
>>> my_list.map(lambda x: x + 1)
Node(2, Node(3, Empty()))

# pure
>>> List.pure(1)
Node(1, Empty())

# ap
>>> Node(lambda x: x + 1, Empty()).ap(my_list)
Node(2, Node(3, Empty()))

# flatMap
>>> my_list.flatMap(lambda x: Node(x, Node(x + 1, Empty())))
Node(1, Node(2, Node(2, Node(3, Empty()))))
```
#### Maybe
```python
>>> my_just = Just(1)
>>> my_nothing = Nothing()

# map
>>> my_just.map(lambda x: x + 1)
Just(2)
>>> my_nothing.map(lambda x: x + 1)
Nothing()

# pure
>>> Maybe.pure(1)
Just(1)

# ap
>>> Just(lambda x: x + 1).ap(my_just)
Just(2)
>>> Just(lambda x: x + 1).ap(my_nothing)
Nothing()
>>> Nothing().ap(my_just)
Nothing()
>>> Nothing().ap(my_nothing)
Nothing()

# flatMap
>>> my_just.flatMap(lambda x: Just(x + 1))
Just(2)
>>> my_nothing.flatMap(lambda x: Just (x + 1))
Nothing()
```

#### Either
```python
>>> my_left = Left('boohoo')
>>> my_right = Right(1)

# map
>>> my_left.map(lambda x: x + 1)
Left('boohoo')
>>> my_right.map(lambda x: x + 1)
Right(2)

# pure
>>> Either.pure(1)
Right(1)

# ap
>>> Left('sad').ap(my_right)
Left('sad')
>>> Left('sad').ap(my_left)
Left('sad')
>>> Right(lambda x: x + 1).ap(my_right)
Right(2)
>>> Right(lambda x: x + 1).ap(my_left)
Left('boohoo')

# flatMap
>>> my_right.flatMap(lambda x: Right(x + 1))
Right(2)
>>> my_left.flatMap(lambda x: Right(x + 1))
Left('boohoo')
```

#### Validation
```python
>>> my_success = Success(1)
>>> my_failure = Failure(['boohoo'])

# map
>>> my_failure.map(lambda x: x + 1)
Failure(['boohoo'])
>>> my_success.map(lambda x: x + 1)
Right(2)

# pure
>>> Validation.pure(1)
Right(1)

# ap
>>> Failure(['sad']).ap(my_success)
Failure(['sad'])
>>> Failure(['sad']).ap(my_failure)
Failure(['sad', 'boohoo'])
>>> Success(lambda x: x + 1).ap(my_success)
Success(2)
>>> Success(lambda x: x + 1).ap(my_failure)
Failure(['boohoo'])
```

## Question 2
#### Question 2.1: Unsafe Sum
Write a function `sum_digits(n)` that sums the digits of a nonnegative integer \\(n\\). Do so in Python and Haskell. Example runs follow:

```python
>>> sum_digits(1234)
10
>>> sum_digits(99999)
45
```

```haskell
ghci> sumDigits 1234
10
ghci> sumDigits 99999
45
```
#### Question 2.2: Safe Sum
Try entering a negative integer to your functions. My guess is that something bad happens.

Let us make `sum_digits` safe. Re-define `sum_digits` so that we can drop the assumption that \\(n\\) is nonnegative (but will still be an integer), correspondingly using the `Maybe` context to keep our function pure. Use the `Maybe` data structure that you have defined from earlier for the Python version, and use Haskell's `Maybe` in Prelude to do so. Example runs follow:

```python
>>> sum_digits(1234)
Just(10)
>>> sum_digits(99999)
Just(45)
>>> sum_digits(-1)
Nothing
```
```haskell
ghci> sumDigits 1234
Just 10
ghci> sumDigits 99999
Just 45
ghci> sumDigits (-1)
Nothing
```

#### Question 2.3: Final Sum
Now define a function `final_sum(n)` that repeatedly calls `sum_digit` until a single-digit number arises. Just like your safe implementation of `sum_digit`, `final_sum` should also be safe. Example runs follow:
```python
>>> final_sum(1234)
Just(1)
>>> final_sum(99999)
Just(9)
>>> final_sum(-1)
Nothing()
```

```haskell
ghci> finalSum 1234
Just 1
ghci> finalSum 99999
Just 9
ghci> finalSum (-1)
Nothing
```
> Tip: Use `do`-notation in your Haskell implementation!

## Question 3
#### Question 3.1: Splitting Strings
Define a function `split` that splits a string delimited by a character. This is very similar to `s.split(c)` in Python. However, the returned result should be a singly-linked list&mdash;in Python, this would be the singly-linked-list implementation you defined in Question 1, and in Haskell, this would be just `[String]`.

Example runs follow:
```python
>>> split('.', 'hello. world!. hah')
Node('hello', Node(' world!', Node(' hah', Empty())))
>>> split(' ', 'a   b')
Node('this', Node('', Node('', Node('is', Empty()))))
```
```haskell
ghci> split '.' "hello. world!. hah"
["hello"," world!"," hah"]
ghci> split ' ' "a   b"
["a","","","b"]
```

#### Question 3.2: CSV Parsing
The Python `csv` library allows us to read CSV files to give us a list of rows, each row being a list of cells, and each cell is a string. Our goal is to do something similar using the list data structure.

A CSV-string is a string where each row is separated by `\n`, and in each row, each cell is separated by `,`. Our goal is to write a function `csv` that receives a CSV-string and puts all the cells in a two-dimensional list. Example runs follow.

```python
>>> csv('a,b,c\nd,e\nf,g,h')
Node(Node('a', Node('b', Node('c', Empty()))), 
Node(Node('d', Node('e', Empty())), 
Node(Node('f', Node('g', Node('h', Empty()))), 
Empty())))
```

```haskell
ghci> csv "a,b,c\nd,e\nf,g,h"
[["a","b","c"],["d","e"],["f","g","h"]]
```

## Question 4
The notation \\(n\choose k\\) is incredibly useful and has applications in domains like ~gambling~probability and statistics, combinatorics etc. The way to compute \\(n\choose k\\) is straightforward:
\\[\binom{n}{k} = \frac{n!}{k!(n - k)!}\\]

#### Question 4.1: Factorial
Clearly, being able to compute factorials would make computing \\(\binom{n}{k}\\) more convenient. Therefore, write a function `factorial` that computes the factorial of a nonnegative integer. Do so in Python and Haskell. Example runs follow.

```python
>>> factorial(4)
24
>>> factorial(5)
120
```

```haskell
ghci> factorial 4
24
ghci> factorial 5
120
```

#### Question 4.2: Safe Factorial
Just like we have done in Question 2, our goal is to make our functions safer! Re-define `factorial` so that we can drop the assumption that the integer is nonnegative. In addition, your function should receive a name of a variable so that more descriptive error messages can be emitted. Use the `Either` type. Example runs follow:

```python
>>> factorial(4, 'n')
Right(24)
>>> factorial(5, 'k')
Right(120)
>>> factorial(-1, 'n')
Left('n cannot be negative!')
>>> factorial(-1, 'k')
Left('k cannot be negative!')
```

```haskell
ghci> factorial 4 "n"
Right 24
ghci> factorial 5 "k"
Right 120
ghci> factorial (-1) "n"
Left "n cannot be negative!"
ghci> factorial (-1) "k"
Left "k cannot be negative!"
```

#### Question 4.3: Safe n choose k
Now let us use `factorial` to define \\(n\choose k\\)! Use the formula described at the beginning of the question and our `factorial` functions to define a function `choose` that receives integers \\(n\\) and \\(k\\) and returns \\(n\choose k\\). Example runs follow:

```python
>>> choose(5, 2)
Right(10)
>>> choose(-1, -3)
Left('n cannot be negative!')
>>> choose(1, -3)
Left('k cannot be negative!')
>>> choose(3, 6)
Left('n - k cannot be negative!')
```

```haskell
ghci> choose 5 2
Right 10
ghci> choose (-1) (-3)
Left "n cannot be negative!"
ghci> choose 1 (-3)
Left "k cannot be negative!"
ghci> choose 3 6
Left "n - k cannot be negative!"
```

#### Question 4.4: n choose k With Validation
Notice that several things could go wrong with \\(n\choose k\\)! Instead of using `Either`, change the implementation of `factorial` so that it uses the `Validation` applicative instead. This is so that all the error messages are collected. Your `choose` function definition should not change, aside from its type. Example runs follow. 

```python
>>> choose(5, 2)
Success(10)
>>> choose(-1, -3)
Failure(['n cannot be negative!', 'k cannot be negative!'])
>>> choose(1, -3)
Failure(['k cannot be negative!'])
>>> choose(3, 6)
Failure(['n - k cannot be negative!'])
```

```haskell
ghci> choose 5 2
Success 10
ghci> choose (-1) (-3)
Failure ["n cannot be negative!","k cannot be negative!"]
ghci> choose 1 (-3)
Failure ["k cannot be negative!"]
ghci> choose 3 6
Failure ["n - k cannot be negative!"]
```

> Tip: With the `-XApplicativeDo` extension, you can actually use `do` notation on `Functor`s and `Applicative`s. Give it a try by defining `choose` using `do`-notation! For more information on the conditions for when you can use `Applicative` `do`-notation, see the [GHC Users Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/applicative_do.html).
