![Updated][update-shield]
# Context/Notions of Computation

Many popular languages lie to you in many ways. An example is what we have seen earlier, where Python functions do not document exceptions in its type signature, and must be separately annotated as a docstrong to denote as such. This is not including the fact that Python type annotations are not enforced at all.
```python
def happy(x: int) -> int:
    raise Exception("sad!")
```
This is not unique to dynamically-typed languages like Python. This is also the case in Java. In Java, checked exceptions must be explicitly reported in a method signature. However, _unchecked exceptions_, as named, do not need to be reported and are not checked by the Java compiler. That is not to mention other possible "lies", for example, it is possible to return nothing (`null`) even if the method's type signature requires it to return "_something_":

```java
class A {
    String something() {
        return null;
    }
}
```
We can't lie in Haskell. In the first place, we _shouldn't_ lie in general. What now?

Instead, what we can do is to create the right data structures that represent what is _actually_ returned by each function! In the Python example `happy`, what we really wanted to return was _either_ an `int`, _or_ an exception. Let us create a data structure that represents this:
```haskell
data Either a b = Left a  -- sad path
                | Right b -- happy path
```
Furthermore, instead of returning `null` like in Java, we can create a data structure that represents _either_ something, _or_ nothing:
```haskell
data Maybe a = Just a  -- happy path
             | Nothing -- sad path
```

This allows the `happy` and `something` functions to be written safely in Haskell as:
```haskell
happy :: Either String Int
happy = Left "sad!"

something :: Maybe String
something = Nothing
```

The `Maybe` and `Either` types act as _contexts_ or _notions of computation_:
- `Maybe a`&mdash;an `a` or nothing
- `Either a b`&mdash;either `a` or `b`
- `[a]`&mdash;a list of possible `a`s (nondeterminism)
- `IO a`&mdash;an I/O action resulting in `a`

These types allow us to accurately describe what our functions are actually doing! Furthermore, these types "wrap" around a type, i.e. For instance, `Maybe`, `Either a` (for a fixed `a`), `[]` and `IO` all have kind `* -> *`, and essentially provide some _context_ around a type.

Using these types makes programs clearer! For example, we can use `Maybe` to more accurately describe the `head` function, which may return nothing if the input list is empty.
```haskell
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = x
```
Alternatively, we can express the fact that dividing by zero should yield an error:
```haskell
safeDiv :: Int -> Int -> Either String Int
safeDiv x 0 = Left "Cannot divide by zero!"
safediv x y = Right $ x `div` y
```

These data structures allow our functions to act as branching railways!

```
        head'                           safeDiv

        ┏━━━━━ Just a                   ┏━━━━━ Right Int      -- happy path
[a] ━━━━┫                  Int, Int ━━━━┫
        ┗━━━━━ Nothing                  ┗━━━━━ Left String    -- sad path
```

This is the inspiration behind the name "railway pattern", which is the pattern of using algebraic data types to describe the different possible outputs from a function! This is, in fact, a **natural consequence** of purely functional programming. Since functions must be pure, it is not possible to define functions that opaquely cause side-effects. Instead, function signatures must be made _transparent_ by using the right data structures.

What, then, is the _right_ data structure to use? It all depends on the notion of computation that you want to express! If you want to produce nothing in some scenarios, use `Maybe`. If you want to produce something or something else (like an error), use `Either`, so on and so forth!

However, notice that having _functions as railways_ is not very convenient... with the non-railway (and therefore potentially exceptional) `head` function, we could compose `head` with itself, i.e. `head . head :: [[a]] -> a` is perfectly valid. However, we _cannot_ compose `head'` with itself, since `head'` returns a `Maybe a`, which cannot be an argument to `head'`. 

```
    ┏━━━━━      ?          ┏━━━━━
━━━━┫        <----->   ━━━━┫
    ┗━━━━━                 ┗━━━━━
```

How can we make the railway pattern _ergonomic_ enough for us to want to use them? 

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-13%20OCT%202024-57ffd8?style=for-the-badge
