![Updated][update-shield]
# Recursion

Something is recursive if it is defined using itself. A simple (albeit
hardly useful and contrived) example is the following function:

``` python
def f(n):
    return f(n + 1)
```

As defined, the body of function `f` invokes itself. In other
words, it is *defined using itself*. Readers who are unconvinced that
`f` is not a recursive definition may see that it is analogous
to the following mathematical definition, which is clearly recursive:
$$f(n) = f(n + 1) = f(n + 2) = f(n + 3) = \dots$$

Data types can also be defined recursively:

``` python
from abc import ABC
from dataclasses import dataclass

class SinglyLinkedList(ABC):
    pass

class Empty(SinglyLinkedList):
    pass

@dataclass
class Node(SinglyLinkedList):
    head: object
    tail: SinglyLinkedList
```

Likewise, you can see that the `SinglyLinkedList` class has a subclass
`Node` which itself holds another `SinglyLinkedList`. This makes
`SinglyLinkedList` a recursive data structure.

The core idea we present in this section is that we can write recursive
functions by thinking *structural-inductively*.

## Induction

We shall begin by describing a *proof by induction* for a statement over
the natural numbers. The principle of a proof by induction is as
follows: given a *predicate* \\(P(n)\\) over the natural numbers, if we can
show:

1.  \\(P(0)\\) is true
2.  \\(\forall n \in \mathbb{N}.~P(n)\to P(n + 1)\\) (for all natural
    numbers \\(n\\), \\(P(n)\\) implies \\(P(n + 1)\\))

Then \\(P(n)\\) is true for all natural numbers \\(n\\). This works because of
*modus ponens*.

\\[\frac{p~~~~~~~~p\to q}{q} \\text{Modus Ponens}\\]

*Modus Ponens* codifies the following idea: if a proposition \\(p\\) is
true, and if \\(p\\) implies \\(q\\), then \\(q\\) is true. To show how this allows
proofs by induction, we see that we have a proof of \\(P(0)\\). Since we
also know that \\(P(0)\\) implies \\(P(0 + 1) = P(1)\\), by *modus ponens*,
\\(P(1)\\) is true. We also know that \\(P(1)\\) implies \\(P(2)\\), and since from
earlier \\(P(1)\\) is true, by *modus ponens*, \\(P(2)\\) is also true, and so
on.

\\[\frac{P(0)~~~~~~~~\forall k \in \mathbb{N}. P(k)\to P(k + 1)}{\forall n \in \mathbb{N}. P(n)} \\text{Induction}\\]
Let us attempt to write a proof by induction. We start with an
implementation of the factorial function, then prove that it is correct:

``` python
def factorial(n):
    return 1 if not n else \
           n * factorial(n - 1)
```

> _Proposition_. Let \\(P(n)\\) be the proposition that `factorial(n)` returns \\(n!\\).
Then, for all natural numbers \\(n\\), \\(P(n)\\) is true.

*Proof.* We prove \\(P(0)\\) and \\(\forall n\in\mathbb{N}.~P(n)\to P(n + 1)\\)
separately.

**Basis**. Trivial. \\(0! = 1\\). Furthermore, by definition,
`factorial(0)` returns `1`. In other words, \\(P(0)\\) is
true.

**Inductive**. Suppose for some natural number \\(k\\),
`factorial(k)` returns
\\(k! = k \times (k - 1) \times \dots \times 1\\). 
- By definition of `factorial`, `factorial(k + 1)` returns `(k + 1) * factorial(k)`. 
- By our supposition, this evaluates to \\((k + 1) \times k!\\), which is, by definition, \\((k + 1)!\\). 

Thus, if for some \\(k\\), `factorial(k)` returns \\(k!\\), then
`factorial(k + 1)` returns \\((k + 1)!\\). In other words,
\\(\forall k\in\mathbb{N}.~P(k) \to P(k + 1)\\).

As such, since we have proven \\(P(0)\\) and
\\(\forall k\in\mathbb{N}.~P(k)\to P(k+1)\\), we have proven
\\(\forall n\in\mathbb{N}.~P(n)\\) by induction. ◻


## Recursion via Inductive Reasoning

Naturally (haha), the next question to ask would be, "how do we make use
of induction to write recursive functions?" As above, the recipe for a
proof by induction involves (broadly) two steps:

1.  Proof of the basis, e.g. \\(P(0)\\)
2.  The inductive proof, e.g. \\(P(k)\to P(k + 1)\\). Typically, the
    inductive step is completed by **supposing** \\(P(k)\\) for some \\(k\\),
    and showing \\(P(k + 1)\\).

We can write recursive functions similarly by providing:

1.  Non-recursive computation for the result of the base-case, e.g.
    \\(f(0)\\);
2.  Recursive computation of \\(f(k + 1)\\) based on the result of \\(f(k)\\)
    **assuming** that \\(f(k)\\) gives the correct result.

Let us start with a simple description of the natural numbers:
$$\begin{aligned}
  0 &\in \mathbb{N} &&\triangleright~0\text{ is a natural number}\\\\
  n \in \mathbb{N} &\to S(n) \in \mathbb{N} && \triangleright~\text{if }n \text{ is a natural number then it has a successor that is also a natural number}
\end{aligned}$$ 

In our usual understanding of the natural numbers,
\\(S(n) = n + 1\\).

A formulation of the natural numbers in Python might be the following:

``` python
class Nat: pass

@dataclass
class Zero(Nat): pass

@dataclass
class Succ(Nat):
    pred: Nat
```

In which case, the number 3 can be written as follows:

``` python
three = Succ(Succ(Succ(Zero())))
```

Let us attempt to define addition over the natural numbers as we have
formulated above, recursively:

``` python
>>> three = Succ(Succ(Succ(Zero())))
>>> two = Succ(Succ(Zero()))
>>> add(three, two)
Succ(pred=Succ(pred=Succ(pred=Succ(pred=Succ(pred=Zero())))))
```

We might decide to perform recursion on the first addend (doing so on
the second addend is fine as well). In computing \\(m + n\\) there are two
possibilities for what \\(m\\) could be: 
- \\(0\\), or 
- the successor of some natural number \\(k\\). 

The first case is straightforward since \\(0\\) itself is non-recursive (see the definition of `Zero` above), and \\(0 + n\\) is
just \\(n\\). In the other case of \\(m + n\\) where \\(m = S(k)= k + 1\\) for some
\\(k\\), assuming (via our inductive hypothesis) that `add(k, n)`
correctly gives \\(k + n\\), then \\(m + n\\) is \\((k + n) + 1\\) which can be done
by `Succ(add(k, n))`. 

Therefore, we arrive at the following
solution:

``` python
def add(m, n):
    return n if m == Zero() else \
           Succ(add(m.pred, n))
```

Using *structural pattern matching* which we present in
[Chapter 2.4 (Pattern Matching)](../../types/sections/pattern_matching.md), we may also write the following definition which
might be more intuitive:

``` python
def add(m, n):
    match m:
        case Zero(): return n
        case Succ(k): return Succ(add(k, n))
```

At this point you might be wondering why we had given such an odd
formulation of the natural numbers in Python, when we could have used
the `int` type instead (we totally could). One core idea we
would like to make apparent in this formulation, is that recursion via
inductive reasoning can be done over the *structure* of data. Our
formulation shows that natural numbers are recursive data structures,
where the successor of a natural number has a predecessor who is also,
likewise, a natural number. This should make writing recursive functions
over other kinds of recursive data structures not too great of a leap from
writing recursive functions over natural numbers. To show this, consult
our `SinglyLinkedList` data structure from above before we proceed to
write recursive functions over them using inductive reasoning.

First, we shall write a function that appends an element to the end of a
singly-linked list.

```python
>>> append(1, Empty())
Node(head=1,tail=Empty())
>>> append(2, append(1, Empty()))
Node(head=1,tail=Node(head=2,tail=Empty()))
```

We can perform recursion over the structure of the list. There are two
possible structures of the list:
1. The empty list
2. A node of a head element and a tail list 

In the former, we append to an empty list, which should give the singleton. Note once
again that because the empty list is non-recursive, our solution for
appending to the empty list likewise requires no recursion. For the
second case of \\([e_1, e_2,\dots,e_n]\\) (shorthand for
\\(\mathtt{Node}(e_1, [e_2,\dots,e_n])\\)), assume that our solution is
correct for the substructure of the `Node`, i.e.
\\(\mathtt{append}(x, [e_2,\dots,e_n]) = [e_2,\dots,e_n, x]\\). Our goal is
to have
$$\mathtt{append}(x, \mathtt{Node}(e_1, [e_2,\dots,e_n])) = \mathtt{Node}(e_1, [e_2,\dots,e_n,x])$$

Observe that: 

$$\begin{aligned}
  \mathtt{append}(x, \mathtt{Node}(e_1, [e_2,\dots,e_n])) &= \mathtt{Node}(e_1, [e_2,\dots,e_n,x])\\\\
  &= \mathtt{Node}(e_1, \mathtt{append}(x, [e_2,\dots,e_n]))
\end{aligned}$$

Therefore, we can write:

``` python
def append(x, ls):
    if ls == Empty():
        return Node(x, Empty())
    return Node(ls.head, append(x, ls.tail))

# Using structural pattern matching:
def append2(x, ls):
    match ls:
        case Empty():
            return Node(x, Empty())
        case Node(e1, xs):
            return Node(e1, append2(x, xs))
```

We shall give another example by writing list reversals recursively,
going straight into our derivation. Reversing the empty list gives the
empty list. For nonempty lists our goal is to have
\\(\mathtt{reverse}([e_1,\dots,e_n])=[e_n,\dots,e_1]\\). Assuming that
\\(\mathtt{reverse}([e_2,\dots,e_n])=[e_n,\dots,e_2]\\), we can see that
\\([e_n,\dots,e_1] = \mathtt{append}(e_1, [e_n,\dots,e_2])\\), giving us the
following formulation:

``` python
def reverse(ls):
    if ls == Empty():
        return Empty()
    return append(ls.head, reverse(ls.tail))

# Using structural pattern matching:
def reverse2(ls):
    match ls:
        case Empty(): return Empty()
        case Node(e1, xs): return append(e1, reverse2(xs))
```

By this point you should be able to see that recursion can be done via
the following based on the structure of the data:

1.  If the structure of the data is non-recursive, provide a non-recursive computation
    that computes the result directly
2.  If the structure of the data is recursive, recursively solve the problem on the
    substructure(s) of the data (e.g. `pred` or `tail` of the natural
    number or list), and include its result in your main result

You should be well aware that data structures may be more complex. For
example, solving a problem for a structure may require more than one
recursive calls, one non-recursive call and one recursive call, etc. To
make this apparent, let us look at a formulation of a binary tree of
integers:

``` python
class Tree: pass

@dataclass
class EmptyTree(Tree): pass

@dataclass
class TreeNode(Tree):
    left: Tree
    val: int
    right: Tree
```

Now let us attempt to write a function that sums all integers in the
tree. Again there are two possible structures a tree can have: the first
being the empty tree, which has sum 0. For tree nodes, we have two
subtrees, `left` and `right`, from whom we may recursively obtain their
sums using our function. Then, the sum of the entire tree is just the
total of the value at the node, the sum of the left subtree and the sum
of the right subtree:

``` python
def sum_tree(t):
    if t == EmptyTree():
        return 0
    return t.val + sum_tree(t.left) + sum_tree(t.right)

# Structural pattern matching
def sum_tree(t):
    match t:
        case EmptyTree(): return 0
        case TreeNode(l, v, r):
            return sum_tree(l) + v + sum_tree(r)
```

In summary, our formulation of the natural numbers reveals that numbers are also structurally recursive, and therefore, are amenable to recursive computations. We can extend this idea to all recursive structures, which as you will see in these notes, is very common.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
