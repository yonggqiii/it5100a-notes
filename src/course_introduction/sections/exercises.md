![Updated][update-shield]
# Exercises

#### Question 1
Without using GHCI, evaluate the results of
the following expressions:

1.  `3 * 4 + 5`
2.  `3 + 4 * 5`
3.  `` 5 ^ 3 `mod` 4 ``
4.  `97 / 4`
5.  `` 97 `div` 4 ``
6.  `if (let x = 3 in x + 3) /= 5 && 3 < 4 then 1 else 2`
7.  `not otherwise`
8.  `fst (0, 1, 2)`
9.  `succ (1 / 2)`
10. `sqrt 2`
11. `` 1 `elem` [1, 2, 3] ``
12. `let f x = x + 1; g x = x * 2 in (g . f) 1`
13. `[1, 2, 3] ++ [4, 5, 6]`
14. `head [1, 2, 3]`
15. `tail [1, 2, 3]`
16. `init [1, 2, 3]`
17. `[1, 2, 3] !! 0`
18. `null []`
19. `length [1, 2, 3]`
20. `drop 2 [1, 2, 3]`
21. `take 5 [-1..]`
22. `dropWhile even [2, 6, 4, 5, 1, 2, 3]`
23. `sum [fst x | x <- [(i, j) | i <- [1..4], j <- [-1..1]]]`

#### Question 2
Write a function `eqLast` that
receives two nonempty lists and checks whether the last element of both
are the same. Example runs follow:

``` haskell
ghci> eqLast [1,2,3] [4,5]
False
ghci> eqLast "ac" "dc"
True
```

#### Question 3
A palindrome is a word that reads the same forward or backward. Write a function `isPalindrome` that checks if a string is a palindrome. Example runs follow:

``` haskell
ghci> isPalindrome "a"
True
ghci> isPalindrome "bcde"
False
ghci> isPalindrome "racecar"
True
```
#### Question 4
You are writing a function to
determine the cost of a ride. The cost of a ride is determined by
\\(f + rd\\) where \\(f\\) is the flag down fare, \\(r\\) is the per km rate of the
ride and \\(d\\) is the distance of the ride in km. Write a function
`taxiFare` that receives \\(f\\), \\(r\\) and \\(d\\) and computes the
total cost. Example runs follow:

``` haskell
ghci> grab = taxiFare 3 0.5
ghci> gojek = taxiFare 2.5 0.6
ghci> grab 3
4.5
ghci> gojek 3
4.3
ghci> grab 10
8.0
ghci> gojek 10
8.5
```
#### Question 5
Nowadays, we can customize the
food that we order. For example, you can order your burger with extra or
no cheese. In this exercise, we will write a function that takes a string as the
customization and compute the price for burgers with the code names for
the customization. You are given the price list for ingredients:

| Ingredient | Price |
| --- | --- |
| `B` for bun     |   $0.50 |
| `C` for cheese  |   $0.80 |
| `P` for patty   |   $1.50 |
| `V` for veggies |   $0.70 |
| `O` for onions   |  $0.40 |
| `M` for mushrooms | $0.90 |

Write a function `burgerPrice` that takes in a burger as 
a string of characters (each character represents an 
ingredient in the burger) and
returns the price of the burger. While doing so, define an auxilliary
function `ingredientPrice` that receives a single ingredient
(as a character) and returns its price. Define
`ingredientPrice` as part of `burgerPrice` using a
`where` binding. Example runs follow:

``` haskell
ghci> burgerPrice "BVPB"
3.2
ghci> burgerPrice "BVPCOMB"
5.3
```
#### Question 6
Write a function
`sumDigits` that receives a nonnegative integer and gives the
sum of its digits. Example runs follow:

``` haskell
ghci> sumDigits 123
6
ghci> sumDigits 12356
17
```
#### Question 7
Write a function `@:` that
receives a list and a tuple of two values `(start, stop)`, and
performs list slicing with indices starting from `start` and ending at
(and excluding) `stop`. The step size is 1. Assume that both the start
and stop values are nonnegative integers. Example runs follow:

``` haskell
ghci> [1, 2, 3] @: (1, 4)
[2,3]
ghci> [1, 2, 3] @: (4, 1)
[]
ghci> [1, 2, 3] @: (0,1)
[1]
ghci> [1, 2, 3] @: (1,67)
[2,3]
```

Syntactically, the way to define this function might be the following:

``` haskell
ls @: (start, stop) = your implementation here
```



[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20SEP%202024-57ffd8?style=for-the-badge
