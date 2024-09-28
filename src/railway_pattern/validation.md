![Updated][update-shield]
# Validation

One of the most common use of applicatives is _validation_. From our example at the start of this chapter, we have several data structures and we want to be able to parse them from strings:

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
Parsing them from strings may not always succeed, therefore it is imperative that our parsing function does not guarantee that it returns the desired data structure. Therefore, what we can do instead is to have our parsing functions return results in the `Maybe` context to express this fact. This makes our parsing functions have the following type signatures:

```haskell
parseEmail :: String -> Maybe Email
parseSalary :: String -> Maybe Salary
```

Given these functions, we should be able to define a function that parses a `User` from three strings: the user name (which requires no parsing), the email (which is parsed using `parseEmail`) and the salary (which is parsed using `parseSalary`). One way we can implement this `parseUser` function is by receiving the three strings, performing parsing on the `email` and `salary` (in _parallel_[^1]), then constructing our `User` term with the usual `Functor` and `Applicative` methods.

```haskell
parseUser :: String -- name
             -> String -- email
             -> String -- salary
             -> Maybe User -- user
parseUser name email salary =
    let e = parseEmail email
        s = parseSalary salary
    in  User name <$> e <*> s
```

Now our parsing function works just fine!

```haskell
ghci> parseUser "Foo" "yong@qi.com" "1000"
Just (User "Foo" (Email "yong" "qi.com") 1000)
ghci> parseUser "Foo" "yong" "1000"
Nothing
```

## Validation with Error Messages
However, this is not always helpful since when parsing a user, several things could go wrong&mdash;either (1) the supplied email is invalid, (2) the supplied salary is invalid, or (3) both. Therefore, let's have our parsing functions return an error message instead of `Nothing`. For this, what we want to rely on is the `Either` type, which consists of a `Left` of something sad (like an error message), or a `Right` of something happy (the desired result type). We show the definitions of `Either` and its supporting typeclass instances here.

```haskell
data Either a b = Left a -- sad
                | Right b -- happy

instance Functor (Either a) where
    fmap :: (b -> c) -> Either a b -> Either a c
    fmap _ (Left x) = Left x
    fmap f (Right x) = Right $ f x

instance Applicative (Either a) where
    pure :: b -> Either a b
    pure = Right

    (<*>) :: Either a (b -> c) -> Either a b -> Either a c
    Left f <*> _ = Left f
    _ <*> Left x = Left x
    Right f <*> Right x = Right $ f x
```

Let us change the context that our parsing functions will return. Some of the implementation of `parseEmail` and `parseSalary` will need to be changed to add descriptive error messages, and so will their type signatures.

```haskell
parseEmail :: String -> Either String Email
parseEmail email = 
    if ... then
        Left $ "error: " ++ email ++ " is not an email"
    else
        Right $ Email ...

parseSalary :: String -> Either String Salary
parseSalary salary = 
    if ... then
        Left $ "error: " ++ salary ++ " is not a number"
    else
        Right $ SInt ...

```
The great thing is that although we have changed the return types of our individual parsing functions, the implementation of `parseUser` does not, because our definition only relies on the typeclass methods of `Functor` and `Applicative`. Since `Either a` is also an `Applicative`, our definition can be unchanged, and only the type signature of `parseUser` needs to be updated.

```haskell
parseUser :: String -- name
             -> String -- email
             -> String -- salary
             -> Either String User -- user
parseUser name email salary =
    let e = parseEmail email
        s = parseSalary salary
    in  User name <$> e <*> s
```

Now, users of our `parseUser` function will get more descriptive error message reports when parsing fails!

```haskell
ghci> parseUser "Foo" "yong@qi.com" "1000"
Right (User "Foo" (Email "yong" "qi.com") 1000)
ghci> parseUser "Foo" "yong" "1000"
Left "error: yong is not an email"
ghci> parseUser "Foo" "yong@qi.com" "x"
Left "error: x is not a number"
```

## Accumulating Error Messages
However, there is one case that is not handled in our validation function. Let's see what that is:

```haskell
ghci> parseUser "Foo" "abc" "x"
Left "error: abc is not an email"
```
Notice that although _both_ the email and salaries are invalid, the error message shown _only_ highlights the invalid email address. This is misleading because, in fact, the salary is invalid as well, and the user of this function does not know that!

The reason for this lies in the definition of the typeclass instance `Applicative (Either a)`. Notice that in the case of `Left f <*> Left x`, the result is `Left f`, ignoring the other error message `Left x`! In other words, `Either` is a fail-fast `Applicative`, and this is not what we want for our parsing function!

As briefly stated earlier, although the `Applicative` laws describe how an `Applicative` behaves in the _most obvious way_, there is in fact, multiple _most obvious ways_ an instance can behave. In fact, we can define a data structure that does not exhibit fail-fastness, and yet, is still a valid `Applicative`&mdash;the result of which is an `Applicative` that allows us to collect all error messages! Let us give this a try.

The first is to re-define `Either` as an ADT called `Validation` that is practically the same (isomorphic) to `Either`, since that structure is still useful for our purposes. The `Functor` instance of this ADT will remain the same.

```haskell
data Validation err a = Success a
                      | Failure err

instance Functor (Validation err) where
    fmap _ (Failure e) = Failure e
    fmap f (Success x) = Success $ f x
```

Notice that our `err` type variable remains as a type variable, instead of a pre-defined error message collection type like `[String]`. This is because, as always, we want to keep our types as general as possible so that it can be used liberally. However, it is now incumbent on us to restrict or constraint `err` in a way that makes it amenable to collecting error messages in an obvious way so that we can still use it for our purposes. In essence, we just need `err` to have some binary operation that is _associative_:

\\[E_1\oplus(E_2 \oplus E_3) = (E_1 \oplus E_2) \oplus E_3 \\]


For this, we introduce the `Semigroup` typeclass which represents just that!

```haskell
class Semigroup a where
    -- must be associative
    (<>) :: a -> a -> a 
```
Any type is a semigroup as long as it is closed under an associative binary operation. With this, as long as our error is a semigroup, we can use that as our errors in `Validation`! Let us define our `Applicative` instance for this:
```haskell
instance Semigroup err => Applicative (Validation err) where
    pure :: a -> Validation err a
    pure = Success

    (<*>) :: Validation err (a -> b) -> Validation err a -> Validation err b
    Failure l <*> Failure r = Failure (l <> r)
    Failure l <*> _ = Failure l
    _ <*> Failure r = Failure r
    Success f <*> Success x = Success (f x)
```
Notice the double-failure case&mdash;the errors are combined or aggregated using the semigroup binary operation `(<>)`. This way, no information is lost if both operands are `Failure` cases since they are accumulated together.

Assuredly, using a list of strings as our error log is fine because concatenation is an associative binary operation over lists!

```haskell
instance Semigroup [a] where
    (<>) :: [a] -> [a] -> [a]
    (<>) = (++)
```

Therefore, with these definitions we can now amend our parsing functions to use our new `Validation` `Applicative`. First, as per usual, we amend `parseEmail` and `parseUser` so that they correctly use `Validation` instead of `Either`

```haskell
parseEmail :: String -> Validation [String] Email
parseEmail email =
    if ... then
        Failure ["error: " ++ email ++ " is not an email"]
    else
        Success $ Email ...

parseSalary :: String -> Validation [String] Salary
parseSalary salary =
    if ... then
        Failure ["error: " ++ salary ++ " is not a number"]
    else
        Success $ SInt ...
```

Once again, our `parseUser` function does not need to change, except for the type signature.

```haskell
parseUser :: String -- name
             -> String -- email
             -> String -- salary
             -> Validation [String] User -- user
parseUser name email salary =
    let e = parseEmail email
        s = parseSalary salary
    in  User name <$> e <*> s
```

Now, our parsing function works exactly as we want!

```haskell
ghci> parseUser "Foo" "yong@qi.com" "1000"
Success (User "Foo" (Email "yong" "qi.com") 1000)
ghci> parseUser "Foo" "yong" "1000"
Failure ["error: yong is not an email"]
ghci> parseUser "Foo" "yong@qi.com" "x"
Failure ["error: x is not a number"]
ghci> parseUser "Foo" "abc" "x"
Failure ["error: abc is not an email", "error: x is not a number"]
```

## Hands-On
In this chapter, we went from parsing with `Maybe`s to parsing with `Either`s and finally to parsing with `Validation`s. Give this a try for yourself!

Written below is the full program for parsing users with `Maybe`. Try replacing the `Maybe`s with `Either`s, then with `Validation`s and see the outcome of running the program each time!

```haskell
module Main where

import Control.Applicative
import Text.Read
import System.IO

-- edit these!
parseEmail :: String -> Maybe Email
parseEmail email = 
    if '@' `elem` email && length e == 2 && '.' `elem` last e
    -- edit the following two lines when replacing Maybe with
    -- Either or Validation
    then Just $ Email (head e) (last e)
    else Nothing
  where e = split '@' email

parseSalary :: String -> Maybe Salary
parseSalary s = 
  let si = SInt <$> readMaybe s
      sf = SDouble <$> readMaybe s
  in  case si <|> sf of
        Just x -> Just x -- change the RHS `Just x` when replacing
                         -- Maybe with Either or Validation
        Nothing -> Nothing -- change the RHS `Nothing` when replacing
                           -- Maybe with Either or Validation

-- you should only need to change the type of `parseUser` when 
-- replacing Maybe with Either or Validation
parseUser :: String -- name
          -> String -- email
          -> String -- salary
          -> Maybe User 
parseUser name email salary = 
    let e = parseEmail email
        s = parseSalary salary
    in  User name <$> e <*> s

-- no need to edit the rest!

-- the data structures
data Email = Email { emailUsername :: String,
                     emailDomain :: String    }
  deriving (Eq, Show)

data Salary = SInt Int | SDouble Double
  deriving (Eq, Show)

data User = User { username :: String,
                   userEmail :: Email,
                   userSalary :: Salary }
  deriving (Eq, Show)

-- user input with a prompt
input :: String -> IO String
input prompt = do
  putStr prompt
  hFlush stdout
  getLine

-- splitting strings
split :: Char -> String -> [String]
split _ [] = [""]
split delim (x : xs)
    | x == delim = "" : xs'
    | otherwise  = (x : head xs') : tail xs'
  where xs' = split delim xs

-- validation
data Validation err a = Success a
                      | Failure err
    deriving (Eq, Show)

instance Functor (Validation err) where
    fmap :: (a -> b) -> Validation err a -> Validation err b
    fmap _ (Failure e) = Failure e
    fmap f (Success x) = Success $ f x

instance Semigroup err => Applicative (Validation err) where
    pure :: a -> Validation err a
    pure = Success

    (<*>) :: Validation err (a -> b) -> Validation err a -> Validation err b
    Failure l <*> Failure r = Failure (l <> r)
    Failure l <*> _ = Failure l
    _ <*> Failure r = Failure r
    Success f <*> Success x = Success (f x)

main :: IO ()
main = do
  n <- input "Enter name: "
  e <- input "Enter email: "
  s <- input "Enter salary: "
  print $ parseUser n e s
```

---

[^1]: It is important to note that the use of the word "parallel" in this chapter has nothing to do with _parallelism_. The word "parallel" is only used to describe the notion of merging parallel railways into a single rail line via `<*>`. 


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-28%20SEP%202024-57ffd8?style=for-the-badge
