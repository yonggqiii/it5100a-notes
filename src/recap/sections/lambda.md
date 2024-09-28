![Updated][update-shield]

# Lambda Calculus
The \\(\lambda\\) calculus, invented by Alonzo Church, is, essentially, one of the simplest formal "programming" languages. It has a simple _syntax_ and _semantics_ for how programs are evaluated.

## Syntax
Let us first consider the _untyped_ \\(\lambda\\) _calculus_ containing variables, atoms[^1], abstractions and applications. The syntax of \\(\lambda\\) terms `e` in the the untyped \\(\lambda\\) calculus is shown here:

```
e ::= v      > variables like x, y and z
   |  a      > atoms like 1, 2, True, +, *
   |  λv.e   > function abstraction, such as def f(v): return e
   |  e e'   > function call a.k.a function application such as e(e')
```

Part of the motivation for this new language is for expressing higher-order functions. For example, if we wanted to define a function like:

```python
def add(x):
    def g(y):
        return x + y
    return g
```
Doing so mathematically might be a little clumsy. Instead, with the \\(\lambda\\) calculus, we can write it like so:

\\[\textit{add} = \lambda x.\lambda y. x + y\\]

Just like in Python, the scope of a \\(\lambda\\) abstraction extends as far to the right as possible, so the function above should be read as:

\\[\textit{add} = \lambda x.(\lambda y. (x + y))\\]

We show the correspondence between terms in the \\(\lambda\\) calculus with lambda expressions in Python:

| \\(\lambda\\) term | Python Expression |
| --- | --- |
| \\(\lambda x. x + 1\\) | `lambda x: x + 1` |
| \\(\lambda x. \lambda y. x~y\\) | `lambda x: lambda y: x(y)` |
| \\((\lambda x. 2 \times x)~y\\) | `(lambda x: 2 * x)(y)` |

Function applications are left-associative, therefore \\(e1~e2~e3\\) should be read as \\((e1 ~ e2) ~ e3\\), and in Python, `e1(e2)(e3)` should be read as `(e1(e2))(e3)`.

## Semantics
To begin describing how \\(\lambda\\) calculus executes a program (which is really just a \\(\lambda\\) term), we first distinguish between _free_ and _bound_ variables in a \\(\lambda\\) term.

> Definition 1 (Free Variables). A variable \\(x\\) in a \\(\lambda\\) term \\(e\\) is 
> - _bound_ if it is in the scope of a \\(\lambda x\\) in \\(e\\)
> - _free_ otherwise

Then, the functions \\(BV\\) and \\(FV\\) produce the bound and free variables of a \\(\lambda\\) term respectively. For example, \\(FV(\lambda x. \lambda y. x ~ y ~ z) = \\{z\\}\\).

Now we want to be able to perform _substitutions_ of variables with terms. For example, when we have an application of the form \\((\lambda x. e_1) ~ e_2\\), what we want is for \\(e_2\\) to be substituted with \\(x\\) in \\(e_1\\), just like the following function in Python:

```python
def f(x): return x + 1
f(2) # becomes 2 + 1 which is 3, because we substituted x with 2
```
However, this is not straightforward because we may introduce name clashes. For example, if we had \\((\lambda x. \lambda y. x ~ y) y\\), performing a function call with naive substitution gives us \\(\lambda y. y ~ y\\) which is wrong, because now the free variable \\(x\\) is substituted with the _bound_ variable \\(y\\), so the meaning is not preserved. As such, we define _substitutions_ on \\(\lambda\\) terms keeping this in mind.

> Definition 2 (Substitution). \\(e_1[x := e_2]\\) is the substitution of all _free_ occurrences of \\(x\\) in \\(e_1\\) with \\(e_2\\), changing the names of bound variables to avoid name clashes. Substitution is defined by the following rules:
> 1. \\(x[x := e] \equiv e\\)
> 2. \\(a[x := e] \equiv a\\) where \\(a\\) is an atom
> 3. \\((e_1 ~ e_2)[x := e_3] \equiv (e_1[x := e_3])(e_2[x := e_3])\\) 
> 4. \\((\lambda x.e_1)[x := e_2] \equiv \lambda x.e_1\\) since \\(x\\) is not free
> 5. \\((\lambda y.e_1)[x := e_2] \equiv \lambda y.e_1\\) if \\(x \notin FV(e_1)\\)
> 6. \\((\lambda y.e_1)[x := e_2] \equiv \lambda y.(e_1[x:=e_2])\\) if \\(x \in FV(e_1)\\) and \\(y\notin FV(e_2)\\)
> 7. \\((\lambda y.e_1)[x := e_2] \equiv \lambda z.(e_1[y:=z][x := e_2])\\) if \\(x \in FV(e_1)\\) and \\(y\in FV(e_2)\\)

We give some example applications of each rule:
1. \\(x[x := \lambda x. x] \equiv \lambda x. x \\)
2. \\(1[x := \lambda x. x] \equiv 1 \\)
3. \\((x ~ y)[x := z] \equiv z ~ y\\)
4. \\((\lambda x. x+1)[x := y] \equiv \lambda x. x + 1\\)
5. \\((\lambda y. \lambda x. x+y)[x := z] \equiv \lambda y. \lambda x. x+y\\)
6. \\((\lambda y. x+y)[x := z] \equiv \lambda y. z+y\\)
7. \\((\lambda y. x+y)[x := y] \equiv \lambda z. y+z\\) (rename \\(y\\) to \\(z\\) before performing substitution)

The last rule where variables are renamed to avoid name clashes introduces a form of equivalence known as \\(\alpha\\) congruence. It captures the idea that renaming parameters in functions does not change its meaning. For example, the two functions below are, in operation, identical:

```python
def f(x):
    return x + 1
def f(y):
    return y + 1
```
In other words, if two terms differ only in the name of the bound variables, they are said to be \\(\alpha\\) congruent.

Finally, we get to the actual semantics of \\(\lambda\\) calculus, which is described by \\(\beta\\) reduction. Essentially it is as we have briefly described earlier&mdash;a function application `(lambda x: e1)(e2)`, evaluates to `e1` where `x` is substituted with `e2`:

\\[(\lambda x. e) ~ y \triangleright_\beta e[x := y]\\]

For example: 

\\[\begin{align*}
(\lambda x.\lambda y. x ~ y)(\lambda x. x + 1)(2) &\triangleright_\beta (\lambda y. x ~ y)[x := \lambda x. x + 1] (2)\\\\
 & \equiv (\lambda y. (\lambda x. x + 1) ~ y)(2)\\\\
& \triangleright_\beta ((\lambda x. x + 1) ~ y)[y := 2]\\\\
& \equiv (\lambda x. x + 1)(2)\\\\
& \triangleright_\beta (x + 1)[x := 2]\\\\
& \equiv 2 + 1 \\\\
& \equiv 3
\end{align*}
\\]

This is more-or-less how Python evaluates function calls:
```python
>>> (lambda x: lambda y: x(y))(lambda x: x + 1)(2)
3
```

## Typed Variants
Python has types, which describes the class from which an object was instantiated:
```python-repl
>>> type(1)
<class 'int'>
>>> type(1.0)
<class 'float'>
```
We will describe more on types in [Chapter 2 (Types)](../../types/README.md). But for now, know that we can also assign types to terms in the \\(\lambda\\) calculus, giving us new forms of \\(\lambda\\) calculi. The simplest type system we can add to the \\(\lambda\\) calculus is, well, simple types, forming the _simply-typed_ \\(\lambda\\) _calculus_.

For now, we shall restrict types to be the base types to only include `int`, giving us a new language for the calculus:

```
Terms
e ::= v         > variables like x, y and z
   |  a         > atoms like 1, 2, +, *
   |  λv: t.e   > function abstraction, such as def f(v: t): return e
   |  e e'      > function call a.k.a function application such as e(e')

Types
t ::= int         > base type constants, only including integers
   |  t -> t'     > type of functions; -> is right-associative
```

The introduction of types to the calculus now adds the notion of _well-typedness_ to the language. Specifically, not all terms in the untyped \\(\lambda\\) calculus are well-typed in the simply typed \\(\lambda\\) calculus. To formalize this notion of well-typedness, we define _typing rules_ that dictate when a term is well-typed, and what type a term has.

First we have _typing environments_ \\(\Gamma,\Delta,\dots,\\) which are sets (or sometimes lists) of _typing assumptions_ of the form \\(x:\tau\\), stating that we are assuming that \\(x\\) has type \\(\tau\\). Then, the _typing relation_ \\(\Gamma\vdash e: \tau\\) states that in the context \\(\Gamma\\), the term \\(e\\) has type \\(\tau\\). The reason we need typing environments is so that the types of in-scope bound variables in \\(\lambda\\) terms are captured and can be used in the derivation of the types of terms. Instances of typing relations are known as _typing judgements_.

The validity of a typing judgement is shown by providing a _typing derivation_ that is constructed using _typing rules_, which are inference rules:

\\[\frac{A_1 ~ A_2 ~ \dots ~ A_n}{B}\\]

Which basically states that if all the statements \\(A_i\\) are valid, then the statement \\(B\\) is also valid.

Then, the simply-typed \\(\lambda\\) calculus uses the following rules.
1. If a variable \\(x\\) has type \\(\tau\\) in \\(\Gamma\\) then in the context \\(\Gamma\\), \\(x\\) has type \\(\tau\\)
\\[
\frac{x:\tau \in \Gamma}{\Gamma \vdash x : \tau}
\\]
2. If an atom \\(a\\) has type \\(\tau\\) then we can also judge the type of \\(a\\) accordingly
\\[
\frac{a\text{ is an atom of type }\tau}{\Gamma \vdash a: \tau}
\\]
3. Abstraction: If in a certain context we can assume that \\(x\\) has type \\(\tau_1\\) to conclude \\(e\\) has type \\(\tau_2\\), then the same context without this assumption shows that \\(\lambda x:\tau_1.e\\) has type \\(\tau_1\to\tau_2\\)
\\[
\frac{\Gamma,x:\tau_1\vdash e:\tau_2}{\Gamma \vdash (\lambda x:\tau_1.e) : \tau_1 \to \tau_2}
\\]
4. Application: If in a certain context \\(e_1\\) has type \\(\tau_1\to\tau_2\\) and \\(e_2\\) has type \\(\tau_1\\), then \\(e_1 ~ e_2\\) has type \\(\tau_2\\)
\\[
\frac{\Gamma \vdash e_1: \tau_1\to\tau_2 ~~~~~~ \Gamma \vdash e_2: \tau_1}{\Gamma \vdash (e_1 ~ e_2) :\tau_2}
\\]

These rules can be used to perform _type checking_ (the procedure of checking the well-typedness of a term) or _type reconstruction_ (the procedure of finding the types of terms where their typing information is not present, as is the case in the untyped \\(\lambda\\) calculus).

For example, in our calculus we can show that \\(\lambda x: \mathtt{int}\to\mathtt{int}. \lambda y:\mathtt{int}.x ~ y\\) has type \\((\mathtt{int}\to\mathtt{int})\to\mathtt{int}\to\mathtt{int}\\) and is therefore a well-typed term:

\\[
\frac{x:\mathtt{int}\to\mathtt{int} \in \Gamma,x:\mathtt{int}\to\mathtt{int},y:\mathtt{int}}{\Gamma, x: \mathtt{int}\to\mathtt{int}, y:\mathtt{int}\vdash x:\mathtt{int}\to\mathtt{int}}
\\]

\\[
\frac{y:\mathtt{int}\in \Gamma,x:\mathtt{int}\to\mathtt{int},y:\mathtt{int}}{\Gamma, x: \mathtt{int}\to\mathtt{int}, y:\mathtt{int}\vdash y:\mathtt{int}}
\\]

\\[
\frac{\Gamma,x:\mathtt{int}\to\mathtt{int},y:\mathtt{int}\vdash x: \mathtt{int}\to\mathtt{int}~~~~~~~ \Gamma,x:\mathtt{int}\to\mathtt{int},y:\mathtt{int}\vdash y: \mathtt{int}}{\Gamma, x: \mathtt{int}\to\mathtt{int}, y:\mathtt{int}\vdash (x ~ y):\mathtt{int}}
\\]

\\[
\frac{\Gamma, x: \mathtt{int}\to\mathtt{int}, y:\mathtt{int}\vdash (x ~ y):\mathtt{int}}{\Gamma, x: \mathtt{int} \to \mathtt{int} \vdash (\lambda y: \mathtt{int}. x ~ y) : \mathtt{int}\to\mathtt{int}}
\\]

\\[
\frac{\Gamma, x: \mathtt{int} \to \mathtt{int} \vdash (\lambda y: \mathtt{int}. x ~ y) : \mathtt{int}\to\mathtt{int}}{\Gamma\vdash (\lambda x: \mathtt{int} \to \mathtt{int}.\lambda y: \mathtt{int}. x ~ y) : (\mathtt{int}\to\mathtt{int})\to\mathtt{int}\to\mathtt{int}}
\\]

That means the following lambda expression in Python (assuming only `int` exists as a base type) will have the same type:
```python
>>> f = lambda x: lambda y: x(y) # (int -> int) -> int -> int
>>> my_fn = lambda x: x + 1 # int -> int
# f(my_fn): int -> int
# f(my_fn)(3): int
>>> f(my_fn)(3) # int
4
>>> type(f(my_fn)(3))
class <'int'>
```

---
[^1]: The actual untyped \\(\lambda\\) calculus does not have atoms like numbers, booleans etc. However, for simplicity's sake we shall include them in the language. The version we present is frequently termed the _applied_ \\(\lambda\\) _calculus_, in contrast with usual presentations known as the _pure_ \\(\lambda\\) _calculus_ which omits atoms.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
