![Updated][update-shield]
# Railways

One of the core ideas in FP is _composition_, i.e. that to "do one computation after the other" is to _compose_ these computations. In mathematics, function composition is straightforward, given by:
\\[(g\circ f)(x) = g(f(x)) \\]

That is, \\(g\circ f\\) is the function "\\(g\\) _after_ \\(f\\)", which applies \\(f\\) onto \\(x\\), _and then_ apply \\(g\\) on the result.

In an ideal world, composing functions is as straightforward as we have described.

```python
def add_one(x: int) -> int:
    return x + 1
def double(x: int) -> int:
    return x * 2
def div_three(x: int) -> float:
    return x / 3

print(div_three(double(add_one(4))))
```

However, things are rarely perfect. Let us take the following example of an application containing users, with several data structures to represent them.

First, we describe the `User` and `Email` classes:

```python
from dataclasses import dataclass

@dataclass
class Email:
    name: str
    domain: str

@dataclass
class User:
    username: str
    email: Email
    salary: int | float
```

Now, we want to be able to parse user information that is provided as a string. However, note that this parsing may _fail_, therefore we raise exceptions if the input string cannot be parsed as the desired data structure.

```python
def parse_email(s: str) -> Email:
    if '@' not in s:
        raise ValueError
    s = s.split('@')
    if len(s) != 2 or '.' not in s[1]:
        raise ValueError
    return Email(s[0], s[1])

def parse_salary(s: str) -> int | float:
    try:
        return int(s)
    except:
        return float(s) # if this fails and raises an exception,
                        # then do not catch it
```

And to use these functions, we have to ensure that every program point that uses them must be wrapped in a `try` and `except` clause:

```python
def main():
    n = input('Enter name: ')
    e = input('Enter email: ')
    s = input('Enter salary: ')
    try:
        print(User(n, parse_email(e), parse_salary(s)))
    except:
        print('Some error occurred')
```
As you can see, exceptions are being thrown everywhere. Generally, it is hard to keep track of which functions raise/handle execptions, and also hard to compose exceptional functions! Worse still, if the program is poorly documented (as is the case for our example), no one actually knows that `parse_salary` and `parse_email` will raise exceptions!

There is a better way to do this&mdash;by using the _railway pattern_! Let us write the equivalent of the program above with idiomatic Haskell. First, the data structures:

```haskell
data Email = Email { emailUsername :: String
                   , emailDomain   :: String }
  deriving (Eq, Show)

data Salary = SInt Int 
            | SDouble Double
  deriving (Eq, Show)

data User = User { username   :: String
                 , userEmail  :: Email
                 , userSalary :: Salary }
  deriving (Eq, Show)
```

Now, some _magic_. No exceptions are raised in any of the following functions (which at this point, might look like _moon runes_):

```haskell
parseEmail :: String -> Maybe Email
parseEmail email = do
    guard $ '@' `elem` email && length e == 2 && '.' `elem` last e
    return $ Email (head e) (last e)
  where e = split '@' email

parseSalary :: String -> Maybe Salary
parseSalary s = 
  let si = SInt <$> readMaybe s
      sf = SDouble <$> readMaybe s
  in  si <|> sf
```

And the equivalent of `main` in Haskell is shown below.[^1] Although not apparent at this point, we are _guaranteed_ that no exceptions will be raised from using `parseEmail` and `parseSalary`.

```haskell
main :: IO ()
main = do
  n <- input "Enter name: "
  e <- input "Enter email: "
  s <- input "Enter salary: "
  let u = User n <$> parseEmail e <*> parseSalary s
  putStrLn $ maybe "Some error occurred" show u
```

How does this work? The core idea behind the railway pattern is that functions are _pure_ and _statically-typed_, therefore, all functions must make _explicit_ the kind of _effects_ it wants to produce. For this reason, any "exceptions" that it could raise must be _explicitly_ stated in its type signature by returning the appropriate term whose type represents some _notion of computation_. Then, any other function that uses these functions with notions of computation must _explicitly_ handle those notions of computations appropriately.

In this chapter, we describe some of the core facets of the railway pattern:
- What is it?
- What data structures and functions can we use to support this?
- How do we write programs with the railway pattern?

---
[^1]: Wait... is this an _imperative_ program in Haskell?

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
