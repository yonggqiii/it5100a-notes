# Exercises

::: exercise
[]{#q:whatarethetypestypeclasses label="q:whatarethetypestypeclasses"}
Without using GHCI, determine the types of the following expressions:

1.  `1 + 2 * 3`{.haskell}

2.  `(show . )`{.haskell}

3.  `( . show)`{.haskell}

4.  `\ (a, b) -> a == b`{.haskell}
:::

::: exercise
[]{#q:protocols label="q:protocols"} You are given the following untyped
program:

``` python
type Tree[a] = Empty | TreeNode[a]
type List[a] = Empty | ListNode[a]

@dataclass
class Empty:
    def to_list(self):
        return []

@dataclass
class ListNode[a]:
    head: a
    tail: List[a]
    def to_list(self):
        return [self.head] + self.tail.to_list()

@dataclass
class TreeNode[a]:
    l: Tree[a]
    v: a
    r: Tree[a]
    def to_list(self):
      return self.l.to_list() + [self.v] + self.r.to_list()

def flatten(ls):
    if not ls: return []
    return ls[0].to_list() + flatten(ls[1:])

ls = [ListNode(1, Empty()), TreeNode(Empty(), 2, Empty())]
ls2 = flatten(ls)
```

Fill in the type signatures of all the methods and functions and the
type annotations for the `ls`{.python} and `ls2`{.python} variables so
that the type-checker can verify that the program is type-safe. The
given type annotations should be general enough such that defining a new
class and adding an instance of it to `ls`{.python} requires no change
in type annotation:

``` python
@dataclass
class Singleton[a]:
  x: a
  def to_list(self):
    return [self.x]
    
ls = [ListNode(1, Empty()), TreeNode(Empty(), 2, Empty()),
  Singleton(3)]
# ...
```
:::

::: exercise
[]{#q:colours label="q:colours"}
:::

::: exercise
[]{#q:exprtypeclass label="q:exprtypeclass"} Recall in
[\[ch:types\]](#ch:types){reference-type="autoref" reference="ch:types"}
where we defined our `Expr`{.haskell} GADT.

``` haskell
data Expr a where
  LitNumExpr :: Int -> Expr Int
  AddExpr :: Expr Int -> Expr Int -> Expr Int
  -- ...

eval :: Expr a -> a
eval (LitNumExpr x) = x
eval (AddExpr e1 e2) = eval e1 + eval e2
  -- ... 
```

Now that we have learnt typeclasses, let us attempt to *separate* each
constructor of `Expr`{.haskell} as *individual types*, while still
preserving functionality; the purpose of this being to keep the
`Expr`{.haskell} type modular and extensible:

``` haskell
data LitNumExpr = -- ...
data AddExpr = -- ...
```

While still being able to apply `eval`{.haskell} on any of those
expressions:

``` haskell
-- 2 + 3
ghci> eval (AddExpr (LitNumExpr 2) (LitNumExpr 3))
5
-- if 2 == 1 + 1 then 1 + 2 else 4
ghci> eval (CondExpr 
  (EqExpr (LitNumExpr 2) 
          (AddExpr (LitNumExpr 1) (LitNumExpr 1))) 
  (AddExpr (LitNumExpr 1) (LitNumExpr 2))
  (LitNumExpr 4))
3
```

Proceed to define all these different types of expressions and their
corresponding implementations for `eval`{.haskell}:

-   `LitNumExpr`{.haskell}. A literal integer, such as
    `LitNumExpr 3`{.haskell}.

-   `AddExpr`{.haskell}. An addition expression in the form of
    $e_1 + e_2$, such as
    `AddExpr (LitNumExpr 1) (LitNumExpr 2)`{.haskell} representing
    $1 + 2$

-   `EqExpr`{.haskell}. An equality comparison expression in the form of
    $e_1 == e_2$, such as `Eq (LitNumExpr 1) (LitNumExpr 2)`{.haskell}
    representing $1 = 2$

-   `CondExpr`{.haskell}. A conditional expression in the form of
    $\text{if }e\text{ then } e_1 \text{ else }e_2$
:::

::: exercise
[]{#q:mergesort label="q:mergesort"} Implement the mergesort algorithm
as a function `mergesort`{.haskell}. Ignoring time complexity, your
algorithm should split the list in two, recursively mergesort each half,
and merge the two sorted sublists together. Example runs follow:

``` haskell
ghci> mergesort [5,2,3,1,2]
[1,2,2,3,5]
ghci> mergesort "edcba"
"abcde"
```
:::
