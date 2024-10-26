![Updated][update-shield]

# The Existential Typeclass Antipattern

In Python, as long as a class abides by a protocol, the Python type system presumes that this class is a _subclass_ of said protocol. Therefore, any object instantiated from such a class is also considered to be of the same type as the protocol. Thus, in our earlier example, shapes, houses and rooms are all considered to be the same type has `HasArea`.

```python
class HasArea(Protocol):
    def area(self) -> float:
        pass

@dataclass
class Rectangle:
    # ...
    def area(self) -> float:
        return # ...

@dataclass
class House:
    # ...
    def area(self) -> float:
        return # ...

# the following is ok and well-typed
ls: list[HasArea] = [Rectangle(1, 2), House(...)]
```

However, this is _not_ okay in Haskell because `HasArea` is not a type, but a typeclass!

```haskell
x = Triangle 2 3
y = R "bedroom" (Rectangle 3 4)
z = H [y]
ls = [x, y, z] -- error!
```

One question we might ask is, how do we replicate this ability in Python? I.e., how do we create a type that represents all types that implement `HasArea` in Haskell?

## Existential Types
Recall that polymorphic types are also called for-all types. Essentially, the definition of the type is independent of the type parameter. The idea behind for-all types is that we can substitute the type parameter with any other type to give a new type. For example, we know that the `id` function has type `forall a. a -> a`. Therefore, we can apply `id` onto a type, say `Int`, to give us a new function whose type is `Int -> Int`.

The type variable `a` is opaque to whoever defines the term of the polymorphic type. For example, when we define a polymorphic function:

```haskell
singleton :: forall. a -> [a]
singleton x = []
```
The type of `x` is just `a` where we have no idea what `a` is. Thus, the implementation of `singleton` cannot make use of any knowledge of what `a` is because it is just an opaque type variable. In contrast, anyone who _uses_ `singleton` can _decide_ what type will inhabit `a`:

```haskell
x :: Int
y = singleton @Int x
```
As you can see, the caller of `singleton` can decide to pass in the type `Int`, and thus will know that the function application `singleton @Int x` will evaluate to a term of type `[Int]`.

One question you might ask is, we know that "for all" corresponds to \\(\forall\\) in mathematics. Are there also \\(\exists\\) types? The answer is yes! These are known as _existential types_:
\\[\exists\alpha.\tau\\]

The idea behind existential types is that there is _some_ type which inhabits the existential type variable to give a new type. For example the type \\(\exists\alpha.[\alpha]\\) means _"some"_ list of elements. The term `[1, 2]` can also be treated as having the type \\(\exists\alpha.[\alpha]\\) because we know that we can let \\(\alpha\\) be `Int` and `[1, 2]` is correctly of type `[Int]`. Similarly, `"abc"` can also be treated as having the type \\(\exists\alpha.[\alpha]\\) because we know that we can let \\(\alpha\\) be `Char` and `"abc"` is correctly of type `[Char]`. However, `[1, 'a']` is _not_ of type \\(\exists\alpha.[\alpha]\\) since we cannot assign any type to \\(\alpha\\) so that the type of `[1, 'a']` matches it.

An existential type reverses the relationship of type variable opacity. Recall that the implementer of a polymorphic function sees the type variable as opaque, while the user gets to decide what type inhabits the type variable. For an existential type, the implementer gets to decide what type inhabits the type variable, while the user of an existential type views the type variable as opaque.

> **Polymorphism**: implementer **does not know the type, must ignore it**. **User** chooses the type.
>
> **Existential types**: implementer **chooses the type**. **User** does not know the type, must ignore it.

Ideally, this allows us to define a type of lists \\([\exists\alpha.\mathtt{HasArea}~\alpha\Rightarrow\alpha]\\) (read: a list of elements, each of which are some \\(\alpha\\) that implements `HasArea`), however the quantification of the type variable is inside the list constructor; these are called _impredicative_ types. Haskell does not support impredicative types. What can we do now?

What we can try to do is to define a new wrapper type that stores elements of type \\(\exists\alpha.\mathtt{HasArea}~\alpha\\), like so:

```haskell
data HasAreaType = HAT (∃a. HasArea a => a)
instance HasArea HasAreaType where
    area :: HasAreaType -> Double
    area (HAT x) = area x
```
However, perhaps surprisingly given what we've been talking about, Haskell does not even support existential types. What now?

## Mental Model for Existential Types

Just like how we have given a mental model for polymorphism, we give a mental model for existential types. Recall that a polymorphic function is a function that receives a type parameter and returns a function that is specialized over the type parameter. For us, let us suppose that a term of an existential type \\(\exists\alpha.\tau\\) is a pair \\((\beta,x)\\) such that \\(x\\) has type \\(\tau[\alpha:=\beta]\\).

- `(Int, [1, 2])` is a term of type \\(\exists\alpha.[\alpha]\\) because `[1, 2] :: [Int]`
- `(Char, "abc")` is also a term of type \\(\exists\alpha.[\alpha]\\) because `"abc" :: [Char]`

Therefore, a function on an existential type can be thought of as a function receiving a pair, whose first element is a type, and the second element is corresponding term. 

In our example above, the `HAT` constructor would therefore have type

```haskell
HAT :: (∃a. HasArea a => a) -> HasAreaType
```
Using our mental model, we destructure the existential type as a pair:
```haskell
HAT :: (a :: *, HasArea a => a) -> HasAreaType
```
Recall _currying_, where a function over more than one argument is split into a function receiving one parameter and returning a function that receives the rest. We thus curry the `HAT` constructor like so:
```haskell
HAT :: (a :: *) -> HasArea a => a -> HasAreaType
```
Remember what it means for a function that receives a type as a parameter&mdash;this is a **polymorphic function**!
```haskell
HAT :: forall a. HasArea a => a -> HasAreaType
```
Indeed, _polymorphic functions simulate functions over existential types_. Let us show more examples of this being the case. For example, the `area` typeclass method is a function over _something that implements `HasArea`_, and returns a `Double`. Therefore, it should have the following function signature:
```haskell
area :: (∃a. HasArea a => a) -> Double
```
However, we know that we can curry the existential type to get a polymorphic function, allowing us to recover the original type signature!
```haskell
area :: forall a. HasArea a => Double
```

In another example, we know the `EqExpr` constructor from the previous chapter is constructed by providing any two expressions that are amenable to equality comparisons:
```haskell
EqExpr :: (∃a. Eq a => (Expr a, Expr a)) -> Expr Bool
```
Again, with currying, we recover our original type signature for `EqExpr`:
```haskell
EqExpr :: forall a. Eq a => Expr a -> Expr a -> Expr Bool
```

With this in mind, we can now properly create our `HAT` constructor and use the `HasAreaType` type to put shapes, rooms and houses in a single list!

```haskell
data HasAreaType where
    HAT :: forall a. HasArea a => a -> HasAreaType
instance HasArea HasAreaType where
    area :: HasAreaType -> area
    area (HAT x) = area x

x = Triangle 2 3
y = R "bedroom" (Rectangle 3 4)
z = H [y]

ls :: [HasAreaType]
ls = [HAT x, HAT y, HAT z]

d = totalArea ls -- 27
```

## The Antipattern

Notice that we went through this entire journey just so that we can put these different types in a list, which is so that we can compute the total area. However, in this case, we can actually just save the trouble and do this:

```haskell
x = Triangle 2 3
y = R "bedroom" (Rectangle 3 4)
z = H [y]

ls :: [Double]
ls = [area x, area y, area z]

d = sum ls -- 27
```

Of course, there are definitely use cases for existential types like `HasAreaType`. We frequently call these _abstract data types_. However, these are not commonly used. In fact, not knowing what existential types are should **not** affect your understanding of type classes and polymorphic types. In addition, encoding existential types as pairs is _very handwave-y_ and is not even supported in Haskell. The closest analogue of real-world existential types is _dependent pair types_ or \\(\Sigma\\)-_types_, which is different to the existential types we have seen. The demonstration that we have seen so far only serves as a mental model for why we write polymorphic functions were the return type does not depend on the type parameters.

The key point is that we should _not_ immediately attempt to replicate OO design patterns in FP just because they are familiar. Trying to skirt around the restrictions of the type system is, generally, not a good idea (there are cases where that is useful, but such scenarios occur exceedingly infrequently). 

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
