![Updated][update-shield]
# Ad-Hoc Polymorphism

So far, we have learnt how to define algebraic data types, and
construct&mdash;and destructure&mdash;terms of those types. However, algebraic
data types typically only represent data, unlike objects in OOP.
Therefore, we frequently write *functions* acting on terms of those
types. As an example, drawing from [Chapter 2.5 Question 7](../../types/sections/exercises.md), let us define a
`Shape` ADT that represents circles and rectangles.

``` haskell
data Shape = Circle Double
           | Rectangle Double Double
```

On its own, this ADT does not do very much. What we would like to do
additionally, is to define a function over `Shape`s. For
example, a function `area` that computes the area of a
`Shape`:

``` haskell
area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle w h) = w * h
```

However, you might notice that `area` should not be
exclusively defined on `Shape`s; it could very well be the
case that we will later define other algebraic data types from which we
can also compute its area. For example, let us define a
`House` data type that also has a way to compute its area:

``` haskell
data House = H [Room]
type Room = Rectangle

area' :: House -> Double
area' (H ls) = foldr ((+) . area) 0 ls
```

Notice that we cannot, at this point, abstract `area` and
`area'` into a single function because these functions work on
specific types, and they have type-specific implementations. It is such
a waste for us to have to use different names to describe the same idea.

The question then becomes, is it possible for us to define an
`area` function that is polymorphic (not fully parametrically
polymorphic) in some ad-hoc way? That is, can `area` have one
implementation when given an argument of type `Shape`, and
another implementation when given another argument of type
`House`?

## Ad-Hoc Polymorphism in Python

Notice that this is entirely possible in Python and other OO languages,
where different classes can define methods of the same name.

``` python
@dataclass
class Rectangle:
  w: float
  h: float
  def area(self) -> float:
    return self.w * self.h

@dataclass
class Circle:
  r: float
  def area(self) -> float:
    return pi * r ** 2

@dataclass
class House:
  ls: list[Rectangle]
  def area(self) -> float:
    return sum(x.area() for x in self.ls)
```

All of these disparate types can define an `area` method with
its own type-specific implementation, and this is known as method
*overloading*. In fact, Python allows us to use them in an ad-hoc manner
because Python does not enforce types. Therefore, a program like the
following will be totally fine.

``` python
def total_area(ls):
  return sum(x.area() for x in ls)

ls = [Rectangle(1, 2), House([Rectangle(3, 4)])]
print(total_area(ls)) # 14
```

`total_area` works because Python uses *duck typing*&mdash;if it
walks like a duck, quacks like a duck, it is probably a duck. Therefore,
as long as the elements of the input list `ls` defines a method
`area` that returns something that can be summed over, no type
errors will present from program execution.

Python allows us to take this further by defining special methods to
overload operators. For example, we can define the `__add__`
method on any class to define how it should behave under the
`+` operator:

``` python
@dataclass
class Fraction:
  num: int
  den: int
  def __add__(self, f):
    num = self.num * f.den + f.num * self.den
    den = self.den * f.den
    return Fraction(num, den)

print(1 + 2) # 3
print(Fraction(1, 2) + Fraction(3, 4)) # Fraction(10, 8)
```

However, relying on duck typing alone forces us to ditch any hopes for
static type checking. From the definition of the `ls` variable
above:

``` python
ls = [Rectangle(1, 2), House([Rectangle(3, 4)])]
```

based on what we know, `ls` cannot be given a suitable type that is useful. Great
thing is, Python has support for _protocols_ that allow us to group
classes that adhere to a common interface (without the need for class
extension):

``` python
class HasArea(Protocol):
  @abstractmethod
  def area(self) -> float:
    pass

def total_area(ls: list[HasArea]) -> float:
  return sum(x.area() for x in ls)

ls: list[HasArea] = [Rectangle(1, 2), House([Rectangle(3, 4)])]
print(total_area(ls)) # 14
```

This is great because we have the ability to perform ad-hoc polymorphism
without coupling the data with behaviour&mdash;the `HasArea`
protocol makes no mention of its inhabiting classes
`Rectangle`, `Circle` and `House`, and
vice-versa, and yet we have provided enough information for the
type-checker so that bogus code such as the following gets flagged
early.

``` python
ls: list[HasArea] = [1] # Type checker complains about this
print(total_area(ls)) # TypeError
```

## The Expression Problem in Python

There are several limitations of our solution using protocols. Firstly,
Python's type system is not powerful or expressive enough to describe
protocols involving higher-kinded types. Secondly, although we have
earlier achieved decoupling between classes and the protocols they abide
by, we are not able to decouple classes and their methods. If we wanted
to completely decouple them, we would define methods as plain functions,
and run into the same problems we have seen in the Haskell
implementation of `area` and `area'` above.

At the expense of type safety, let us attempt to decouple
`area` and their implementing classes. The idea is to define an
`area` function that receives a helper function that computes
the type specific area of an object:

``` python
def area(x, helper) -> float:
  return helper(x)

def rectangle_area(rect: Rectangle) -> float:
  return rect.w * rect.h
def house_area(house: House) -> float:
  return sum(x.area() for x in house.ls)

r = Rectangle(1, 2)
h = House([Rectangle(3, 4)])
area(r, rectangle_area) # 2
area(h, house_area) # 12
```

This implementation is silly because we could easily remove one level of
indirection by invoking `rectangle_area` or
`house_area` directly. However, notice that the implementations
are specific to classes&mdash;or, types&mdash;thus, what we can do is to store
these helpers in a dictionary whose keys are the types they were meant
to be inhabiting. Then, the `area` function can look up the
right type-specific implementation based on the type of the argument.

``` python
HasArea = {} 

def area(x):
  helper = HasArea[type(x)]
  return helper(x)

HasArea[Rectangle] = lambda x: x.w * x.h
HasArea[House] = lambda house: sum(x.area() for x in house.ls)

r = Rectangle(1, 2)
h = House([Rectangle(3, 4)])
area(r) # 2
area(h) # 12
```

What's great about this approach is that (1) otherwise disparate classes
adhere to a common interface, and (2) the classes and methods are
completely decoupled. We can later on define additional classes and its
type-specific implementation of `area`, or define a
type-specific implementation of `area` for a class that has
already been defined!

``` python
@dataclass
class Triangle:
  w: float
  h: float

HasArea[Triangle] = lambda t: 0.5 * t.w * t.h

area(Triangle(5, 2)) # 5
```

Unfortunately, all of these gains came at the cost of type safety. Is
there a better way to do this? In Haskell, yes&mdash;with typeclasses!

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-10%20OCT%202024-57ffd8?style=for-the-badge
