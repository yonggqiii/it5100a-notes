![Updated][update-shield]

# Typeclasses {#typeclasses}

Typeclasses are a type system construct that enables ad-hoc
polymorphism. Essentially, a typeclass is a nominal classification of
types that all support some specified behaviour, by having each type
providing its type-specific implementation for those behaviours.
Alternatively, a typeclass can be seen as a constraint for a type to
support specified behaviours.

Just like classes in OOP are blueprints for creating instances of the
class (objects), a typeclass is a blueprint for creating typeclass
instances. This time, a typeclass provides the
interface/specification/contract for members of the typeclass to adhere
to, and typeclass instances provide the actual type-specific
implementations of functions specified in the typeclass. In essence, a
typeclass is a constraint over types, and a typeclass instance is a witness that
for types meeting those constraints.

To build on intuition, pretend that there is a super cool magic club,
and members of this club must have a magic assistant and a magic trick.
This club acts as a typeclass. Then suppose cats and dogs want to join
this club. To do so, they must provide proof to the club administrators
(in Haskell, the compiler) that they have a magic assistant and a magic
trick. Suppose that the cats come together with their mouse friends as
their magic assistants, and their magic trick is to cough up a furball,
and the dogs all present their chew toys as their magic assistants, and
their magic trick is to give their paw. The club administrator then puts
all these into boxes as certificates of their membership into the
club&mdash;in our analogy, these certificates are typeclass instances.

Let us return to the shape and house example we have seen at the start
of this chapter. We first define some types (slightly different from
before) that all have an area:

``` haskell
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double
data House = H [Room]
data room = R { roomName :: String
              , shape    :: Shape }
```

Now, our goal is to describe the phenomenon that some types have an
area. For this, we shall describe a contract for such types to follow.
The contract is straightforward&mdash;all such types must have an
`area` function (known as a method).

``` haskell
class HasArea a where
  area :: a -> Double
```

An important question one might ask is: why is `HasArea`
polymorphic? To give an analogy, recall in our Python implementation
with dictionaries that `HasArea` is a dictionary where we are
looking up type-specific implementations of `area` by type.
Essentially, it is a finite map or (partial) function from types to
functions. This essentially makes `HasArea` polymorphic,
because it acts as a function that produces different implementations
depending on the type!

Then, the `area` function should also receive a parameter of
type `a`&mdash;that is, if `a` is a member of the
`HasArea` typeclass, then there is a function
`area :: a -> Double`. The example typeclass instances make
this clear:

``` haskell
instance HasArea Shape where
  area :: Shape -> Double
  area (Circle r) = pi * r ^ 2
  area (Rectangle w h) = w * h
  area (Triangle w h) = w * h / 2

instance HasArea Room where
  area :: Room -> Double
  area x = area $ shape x

instance HasArea House where
  area :: House -> Double
  area (H rooms) = sum $ map area rooms
```

Each instance of `HasArea` provides a type-specific
implementation of `area`. For example, the
`HasArea Shape` instance acts as a witness that
`Shape` belongs to the `HasArea` typeclass. It does
so by providing an implementation of `area :: Shape -> Double`
(in the obvious way). We do the same for rooms and houses, and now the
`area` function works for all (and only) these three types!

``` haskell
x :: Shape = Triangle 2 3
y :: Room = R "bedroom" (Rectangle 3 4)
z :: House = H [y]

ax = area x -- 3
ay = area y -- 12
az = area z -- 12
```

Now let us investigate the type of `area`:

``` haskell
ghci> :t area
area :: forall a. HasArea a => a -> double
```

The type of `area` is read as "a function for all
`a` where `a` is constrained by `HasArea`,
and receives an `a`, and returns a `Double`".

Constrains on type variables are not limited to class methods. In fact,
we can, and probably should, make functions that use `area`
polymorphically over type variables, constrained by `HasArea`. Let
us consider a function that sums the area over a list of shapes, and
another one over a list of rooms:

``` haskell
totalArea :: [Shape] -> Double
totalArea [] = 0
totalArea (x : xs) = area x + totalArea xs

-- alternatively
totalArea' :: [Shape] -> Double
totalArea' = sum . map area

totalArea'' :: [Room] -> Double
totalArea'' = sum . map area
```

Both `totalArea'` and `totalArea''` have
precisely the same implementation, except that they operate over
`Shape` and `Room` respectively. We can
substitute these types for any type variable `a`, so long as
there is an instance of `HasArea a`! Therefore, the most
general type we should ascribe for this function would be

``` haskell
totalArea :: HasArea a => [a] -> Double
totalArea = sum . map area
```

Now our `totalArea` function works on any list that contains a
type that has an instance of `HasArea`!

``` haskell
xs :: [Shape] = [Rectangle 1 2, Triangle 3 4]
ys :: [House] = [H [R "bedroom" (Rectangle 1 2)]]
axs = totalArea xs -- 8
ayx = totalArea ys -- 2
```

### How Typeclasses Work

By now, you should be able to observe that typeclasses allow (1)
otherwise disparate types adhering to a common interface, i.e. ad-hoc
polymorphism and (2) decoupling types and behaviour, all in a type-safe
way&mdash;this is very difficult (if not impossible) to achieve in other
languages like Python. The question then becomes: how does Haskell do
it?

The core idea behind typeclasses and typeclass instances is that
typeclasses are implemented as regular algebraic data types, and
typeclass instances are implemented as regular terms of typeclasses.
Using our `area` example, we can define the typeclass as

``` haskell
data HasArea a = HA { area :: a -> Double }
```

Then, typeclass instances are merely helper-terms of the
`HasArea` type:

``` haskell
hasAreaShape :: HasArea Shape
hasAreaShape = HA $ \x -> case x of
  Circle    r   -> pi * r ^ 2
  Rectangle w h -> w * h
  Triangle  w h -> w * h / 2
```

Notice that `area` now has the type
`HasArea a -> a -> Double`. Clearly,
`area hasAreaShape` is now the `Shape`-specific
implementation for obtaining the area of a shape! We can take this
further by defining the helper-terms for other types that wish to
implement the `HasArea` typeclass:

``` haskell
hasAreaRoom :: HasArea Room
hasAreaRoom = HA $ \x -> area hasAreaShape (shape x)

hasAreaHouse :: HasArea House
hasAreaHouse = HA $ \x -> case x of
  H rooms -> sum $ map (area hasAreaRoom) rooms
```

Finally, we can use the `area` function, together with the
type-specific helpers, to compute the area of shapes, rooms and houses!

``` haskell
x :: Shape = Triangle 2 3
y :: Room = R "bedroom" (Rectangle 3 4)
z :: House = H [y]

ax = area hasAreaShape x -- 3
ay = area hasAreaRoom y -- 12
az = area hasAreamHouse z -- 12
```

This is (more-or-less) how Haskell implements typeclasses and typeclass
instances. The only difference is that the Haskell compiler will
automatically *infer* the helper term when a typeclass method is used,
allowing us to omit them. This *term inference* that Haskell supports
allow us to define and use ad-hoc polymorphic functions in a type-safe
way.


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
