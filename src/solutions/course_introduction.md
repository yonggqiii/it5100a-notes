# Course Introduction

## Question 1

1. `17`. \\((3\times 4) +5 = 12 + 5 = 17\\).
2. `23`. \\(3 + (4 \times 5) = 3 + 20 = 23\\). Note that `*` has higher precedence than `+`.
3. `1`. Exponentiation has a higher precedence than modulo (non-operator functions like `mod` that are called in an infix manner can have a well-defined operator precedence level).
4. `24.25`. Regular division of integers gives a `Fractional` type.
5. `24`. The `div` function is similar to `//` in Python.
6. `1`. First we evaluate the condition `let x = 3 in x + 3` evaluates to `3 + 3` which therefore is `6`. Clearly `6 /= 5` is true, so we need to also evaluate `3 < 4`, which is also true. `&&` is the same as `and` in Python, so `True and True` is therefore `True`. Thus, the whole expression evaluates to the if branch, which is `1`.
7. `False`. `otherwise` is actually just `True` by definition, so `not True` becomes `False`.
8. It actually causes a compile-time error since it is a type error. `fst` and `snd` receive pairs, so these functions do not work on triples.
9. `1.5`. The `succ` function returns the successor of any enumerable type. For numbers, this would be one more than the number.
10. `1.4142135623730951`. Straightforward. Notice that Haskell's Prelude (the built-in stuff) comes with many math functions.
11. `True`. The `elem` function is similar to
    `in` in Python.
12. `4`. When writing `let` bindings in a single line, we can
    separate multiple definitions with `;`. Therefore, we have defined
    two functions `f` and `g` which add one and multiply by 2
    respectively. The `.` operator is function composition, where
    \\((g\circ f)(x) = g(f(x))\\), so `(g . f) 1` is the same as
    `g (f 1)`, which evaluates to `4`.
13. `[1, 2, 3, 4, 5, 6]`. This is straightforward, since
    `++` concatenates two lists.
14. `1`. `head` returns the first element of the
    list.
15. `[2, 3]`. `tail` returns the suffix of the list
    without the first element.
16. `[1, 2]`. `init` returns the list without the
    last element.
17. `1`. `!!` is indexing.
18. `True`. `null` checks whether a list is empty.
19. `3`. Obvious.
20. `[3]`. `drop n` drops the first `n` elements of
    a list.
21. `[-1, 0, 1, 2, 3]`. `take n` takes the first `n`
    elements of a list. The range `[-1..]` is an infinitely
    long range from `-1` to infinity.
22. `[5, 1, 2 ,3]`. `dropWhile f` will drop elements
    from a list until `f` returns false for an element.
23. `30`. The easiest way to see this is by converting this to
    the equivalent Python expression:
    ```python
    >>> sum([x[0] for x in 
                [(i, j) for i in range(1, 5)
                        for j in range(-1, 2)]])
    ```

    Going back to Haskell land, let us evaluate the inner list first.
    `[(i, j) | i <- [1..4], j <- [-1..1]]` gives
    `[(1, -1), (1, 0), (1, 1), (2, -1), ..., (4, 1)]` then,
    `[fst x | x <- ...]` would therefore give
    `[1,1,1,2,2,2,3,3,3,4,4,4]` which sums to 30.

### Question 2
Idea: take the last elements of both lists, and check for
equality. For this, we can use the `last` function.

``` haskell
eqLast xs ys = last xs == last ys
```

### Question 3
Idea: reverse the string, and check if the string and its
reverse are equal. For this, we can use the `reverse`
function.

``` haskell
isPalindrome w = w == reverse w
```

### Question 4

``` haskell
taxiFare f r d = f + r * d
```

### Question 5
There are several ways to approach this problem. Let us
first define the `ingredientPrice` function which should be
straightforward to do.

``` haskell
ingredientPrice i 
  | i == 'B' = 0.5
  | i == 'C' = 0.8
  | i == 'P' = 1.5
  | i == 'V' = 0.7
  | i == 'O' = 0.4
  | i == 'M' = 0.9
```

Then we can define `burgerPrice` recursively. If the string is
empty then the price is 0. Otherwise, take the price of the first
ingredient and add that to the price of the remaining burger.

``` haskell
burgerPrice burger 
  | null burger = 0
  | otherwise = 
      let first = ingredientPrice (head burger)
          rest  = burgerPrice (tail burger)
      in  first + rest
```

Of course, we know that we can do the following in Python quite nicely:

``` python
def burger_price(burger):
    return sum(ingredient_price(i) for i in burger)
```

This can be done in Haskell too as follows:

``` haskell
burgerPrice burger = sum [ingredientPrice i | i <- burger]
```

We can also replace the comprehension expression in Python using
`map`:

``` python
def burger_price(burger):
    return sum(map(ingredient_price, burger))
```

Haskell also has a `map` (or `fmap`) function that
does the same thing:

``` haskell
burgerPrice burger = sum $ map ingredientPrice burger
```

The `$` sign is just regular function application, except that `$` binds
very weakly. So `sum $ map ingredientPrice burger` is basically
`sum (map ingredientPrice burger)`.

Finally, notice that
`burgerPrice x = sum ((map ingredientPrice) x)`, so
effectively we can finally define our function this way:

``` haskell
burgerPrice = sum . map ingredientPrice
  where ingredientPrice i 
          | i == 'B' = 0.5
          | i == 'C' = 0.8
          | i == 'P' = 1.5
          | i == 'V' = 0.7
          | i == 'O' = 0.4
          | i == 'M' = 0.9
```

To see this, let \\(b\\) be `burgerPrice`, \\(g\\) be `sum` and \\(f\\) be
`map ingredientPrice`. We have shown that \\[b(x) = g(f(x))\\] By
definition, \\[b = g\circ f\\]

This style of writing functions is known as _point-free_ style, where functions are expressed 
as a _composition_ of functions.

### Question 6
Again, there are several ways to solve this. To do so
numerically, we can define our function recursively:

\\[s(n) = \begin{cases}
    n & \text{if } n < 10\\\\
    n \mod 10 + s(\lfloor n \div 10 \rfloor)  & \text{otherwise} 
  \end{cases}\\]

``` haskell
sumDigits n
  | n < 10    = n
  | otherwise = n `mod` 10 + sumDigits (n `div` 10)
```

Alternatively, we may convert `n` into a string, convert each character
into integers, then obtain the sum. This might be expressed in Python
as:

``` python
def sum_digits(n):
    return sum(map(int, str(n)))
```

Converting `n` into a string can be done by `show`:

``` haskell
ghci> show 123
"123"
```

Converting back into an integer can be done with `read` (you have to
explicitly state the output type of the `read` function since this can
be ambiguous):

``` haskell
ghci> read "123" :: Int
123
```

However, we can't `read` from **characters** since the `read` function
receives strings. Good thing that strings are lists of characters, so by
putting the character in a list, we now obtain the ability to read a
digit (as a character) as an integer.

``` haskell
ghci> read '1' :: Int
-- error!
ghci> read ['1'] :: Int
1
```

To put things into lists, we can use the `return` function!

``` haskell
ghci> return '1' :: String
"1"
ghci> (read . return) '1' :: Int
1
```

Thus, the `read . return` function allows us to parse each character
into an integer. Combining this with what we had before, we can obtain
the list of the digits (as integers) from `n` using:

``` haskell
ghci> [(read . return) digit | digit <- show 123] :: [Int]
[1, 2, 3]
```

Again, we can use `map` instead of list comprehension.

``` haskell
ghci> map (read . return) (show 123) :: [Int]
[1, 2, 3]
```

Obtaining the sum of this list gives us exactly what we want. Thus, our
`sumDigits` function is succinctly defined as follows:

``` haskell
sumDigits = sum . map (read . return) . show
```

### Question 7
Idea: drop the first `start` elements, then take the
`stop - start` elements after that.

``` haskell
ls @: (start, stop) = take (stop - start) (drop start ls)
```
