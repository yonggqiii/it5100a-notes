![Updated][update-shield]

# Monad Transformers
Monads support _composition in context_. Another question to ask is, can we _compose monads_? In other words, can we combine monads together?

Consider the example of finding the length of the path between two connected neighbours in a directed graph, except that we have each node connected to at most one edge. The way we might solve this problem is, once again, via DFS (which in this case is the same as BFS), except that our graph is now of type `[(Node, Node)]` and our function returns the length of the path instead of a `Bool` value describing whether the path exists:
```haskell
type Node = Int
type Graph = [(Node, Node)]
dfs :: Node -> Node -> Graph -> Maybe Int
dfs src dst gph = aux src [] gph where
    aux :: Node -> [Node] -> Graph -> Maybe Int
    aux current visited gph'
      | arrived = return 0
      | alreadyVisited = Nothing
      | otherwise  = do
          n <- lookup current gph
          (+1) <$> aux n (current : visited) gph'
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```

Notice that just like our previous definition of `dfs`, all our functions such as `dfs` and `lookup` involve some environment which we need to pass around! Let us try changing everything of type `Graph -> Maybe Int` to `Reader Graph (Maybe Int)` and modify our environment to no longer receive the `gph` argument:

```haskell
type Node = Int
type Graph = [(Node, Node)]
dfs :: Node -> Node -> Reader Graph (Maybe Int)
dfs src dst = aux src [] where
    aux :: Node -> [Node] -> Reader Graph (Maybe Int)
    aux current visited 
      | arrived = return 0
      | alreadyVisited = Nothing
      | otherwise  = do
          n <- lookup current
          (+1) <$> aux n (current : visited)
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```
Unfortunately, our code doesn't type check. This is because now our `do` block performs the monadic operations based on the definition of `Reader`, not on `Maybe`! As such, we may need significant rewrites to our function to introduce the `Reader` monad to our `Maybe` computation. 

## Enriching the `Maybe` Monad
Is there a better way? Yes! Let us try defining a new monad `ReaderMaybe` that essentially acts as both the `Reader` and the `Maybe` monads!

```haskell
newtype ReaderMaybe env a = ReaderMaybe { runReaderMaybe :: Reader env (Maybe a) }

instance Functor (ReaderMaybe env) where
  fmap :: (a -> b) -> ReaderMaybe env a -> ReaderMaybe env b
  fmap f (ReaderMaybe ls) = ReaderMaybe $ fmap (fmap f) ls

instance Applicative (ReaderMaybe env) where
  pure :: a -> ReaderMaybe env a
  pure = ReaderMaybe . pure . pure
  (<*>) :: ReaderMaybe env (a -> b) -> ReaderMaybe env a -> ReaderMaybe env b
  (ReaderMaybe f) <*> (ReaderMaybe x) = ReaderMaybe $ do
    maybe_f <- f
    case maybe_f of 
      Nothing -> return Nothing
      Just f' -> do
        maybe_x <- x
        case maybe_x of 
          Nothing -> return Nothing
          Just x' -> return $ Just (f' x')

instance Monad (ReaderMaybe env) where
  return :: a -> ReaderMaybe env a
  return = pure
  (>>=) :: ReaderMaybe env a -> (a -> ReaderMaybe env b) -> ReaderMaybe env b
  (ReaderMaybe ls) >>= f = ReaderMaybe $ do
    m <- ls
    case m of
      Just x -> runReaderMaybe $ f x
      Nothing -> return Nothing
```
All of these methods are tedious to define, however are somewhat straightforward. In particular, it relies on `do` notation on `Reader`s to extract out the `Maybe` values, and performs the usual `Maybe` methods to compose them. 

The result is that we can now make use of this `ReaderMaybe` monad in our `dfs` function:

```haskell
dfs :: Node -> Node -> Graph -> Maybe Int
dfs src dst = runReaderMaybe (aux src []) where
    aux :: Node -> [Node] -> ReaderMaybe Graph Int
    aux current visited 
      | arrived = return 0
      | alreadyVisited = ReaderMaybe $ return Nothing
      | otherwise  = do
          n <- ReaderMaybe $ lookup current 
          (+1) <$> aux n (current : visited)
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```

There are several points worthy of note in our new implementation:
1. Most of this definition is the same as our original definition that works on the `Maybe` monad
2. Because the `aux` function returns a `ReaderMaybe` term which wraps the actual `Reader` function, we write `runReaderMaybe (aux src [])` to expose the actual `Reader Graph (Maybe Int)` function
3. In the `alreadyVisited` case, we cannot write `alreadyVisited = Nothing` since `Nothing` is not of the type `ReaderMaybe Graph Int`; we also cannot just write `return Nothing` since that has type `ReaderMaybe env (Maybe a)`. As such, we have to use `return @(Reader Graph) Nothing`, then wrap it in the `ReaderMaybe` constructor
4. Similar to (3), instead of `lookup current`, we have to wrap it around the `ReaderMaybe` constructor so that instead of having type `Reader Graph (Maybe Int)`, `ReaderMaybe $ lookup current` will have type `ReaderMaybe Graph Int`, which is the correct type to have.

When converting the original implementation based on `Maybe` into the new implementation based on `ReaderMaybe Graph Int`, one tip is to leave the implementation the same and just change the type signature of the functions to use `ReaderMaybe Graph Int` instead of `Graph -> Maybe Int`, then make use of typing information to correct the types in the program; in other words, "let the types guide your programming", like we have done in [Chapter 2 (Types)](../types/README.md)! Furthermore, we are generally assured that everything works as expected because monads behave in the _most obvious way_!

Just like that, we are able to compose the `Reader` monad with the `Maybe` monad! Running `dfs` works exactly as we'd expect:

```haskell
ghci> my_map = [(1, 2), (2, 3), (3, 1)]
ghci> dfs 1 4 my_map
Nothing
ghci> dfs 1 2 my_map
Just 1
ghci> dfs 2 1 my_map
Just 2
```

Now, what if we wanted to enrich the `Maybe` monad with other notions of computation, such as `[]`, `IO` etc? Suppose we follow the same procedure of enriching `Maybe` with `Reader`, but instead by enriching it with `IO`, giving us a new monad `IOMaybe a` which represents `IO (Maybe a)`:


```haskell
newtype IOMaybe a = IOMaybe { runIOMaybe :: IO (Maybe a) }

instance Functor IOMaybe where
  fmap :: (a -> b) -> IOMaybe a -> IOMaybe b
  fmap f (IOMaybe io) = IOMaybe (fmap (fmap f) io)

instance Applicative IOMaybe where
  pure :: a -> IOMaybe a
  pure = IOMaybe . pure . pure
  (<*>) :: IOMaybe (a -> b) -> IOMaybe a -> IOMaybe b
  (IOMaybe f) <*> (IOMaybe x) = IOMaybe $ do
    maybe_f <- f
    case maybe_f of 
      Nothing -> return Nothing
      Just f' -> do
        maybe_x <- x
        case maybe_x of 
          Nothing -> return Nothing
          Just x' -> return $ Just (f' x')

instance Monad IOMaybe where
  return :: a -> IOMaybe a
  return = pure
  (>>=) :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
  (IOMaybe m) >>= f = IOMaybe $ do
    maybe_m <- m
    case maybe_m of
      Just x -> runIOMaybe $ f x
      Nothing -> return Nothing
```
There are several things worth thinking about. Firstly, so far, it appears that we have to re-create new instances for _every_ notion of computation we want to enrich `Maybe` with. Secondly, you might realise that absolutely nothing about the definition of the instances care about the enriching monad. All of the definitions in the methods for `ReaderMaybe` and `IOMaybe` do not mention any `Reader`-specific or `IO`-specific functions. Instead, they all rely on their respective monad binds! Therefore, we can abstract these into a _monad transformer_.

## Monad Transformers

A monad transformer `MonadT m a` enriches `Monad` with `m`. For example, the `MaybeT m a` monad transformer enriches `Maybe` with `m`. Therefore, our `ReaderMaybe` and `IOMaybe` monads can be represented exactly as `MaybeT (Reader env)` and `MaybeT IO`! The definition of `MaybeT` is virtually the exact same as the definitions of `ReaderMaybe` and `IOMaybe`, except that we do not refer to `Reader` or `IO`, and leave them as `m`:

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))

instance (Monad m) => Monad (MaybeT m) where
    return = MaybeT . return . Just
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
```

With this `Maybe` monad transformer, we can rewrite our definition of `dfs` by replacing `ReaderMaybe Graph Int` with `MaybeT (Reader Graph) Int`!


```haskell
dfs :: Node -> Node -> Graph -> Maybe Int
dfs src dst = runMaybeT (aux src []) where
    aux :: Node -> [Node] -> MaybeT (Reader Graph) Int
    aux current visited 
      | arrived = return 0
      | alreadyVisited = MaybeT $ return Nothing
      | otherwise  = do
          n <- MaybeT $ lookup current 
          (+1) <$> aux n (current : visited)
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```

And now with the `MaybeT` monad transformer, we can enrich the `Maybe` monad with any other monad we want without having to redefine new types and new type class instances for each of the monads we are enriching `Maybe` with!

## Monad Transformer Library

Because monads are so common in programming, the common monads already have their own monad transformers, and these are defined in the [`transformers`](https://hackage.haskell.org/package/transformers) and [`mtl`](https://hackage.haskell.org/package/mtl) libraries. If you want to use these commonly used monad transformers, just download the dependencies and `import` the libraries into your programs! But... how do we do that?

### Build Tools and Package Managers
Most production programming languages have a package manager and build tool, and Haskell is no different. In fact, Haskell has _several_ package managers and build tools you can use. Two of the main competing ones are [`cabal`](https://www.haskell.org/cabal/) and [`stack`](https://docs.haskellstack.org/en/stable/), both of which can be installed via [GHCup](https://www.haskell.org/ghcup/). For our purposes, we shall just use `cabal` since it is slightly simpler to use; most modern versions are generally fine, but for us, we shall use (at least) `cabal-3.10.3`.

### Project Initialization
Using `cabal` is very simple. First, to create a new Haskell project, create an empty directory and run `cabal init` (`>` is the shell prompt of the terminal, do not enter `>` as part of the command)
```output info
> mkdir my-project
> cd my-project
> cabal init
```
Then, `cabal` will take you through a series of questions to initialize the project. Some notable options are:
- Executables are programs that can be executed; libraries are code that other Haskell users can import. For us, choose to build an executable
- The main module of the executable should be `Main.hs`. The `Main.lhs` option is for writing [literate Haskell](https://wiki.haskell.org/Literate_programming) programs. You can use that as well, although for us, it is significantly easier to just use `Main.hs` and write plain Haskell programs.
- The language for our executable should be GHC2021, giving us as many of the latest features as we can have without having to include them as language extensions.

The result of running `cabal init` is that your project directory has been initialized with several parts:
- The `app` directory (or whatever name you have chosen) stores the source code of your program
- `my-project.cabal` is the specification of your project.

### Project Configuration
Let us investigate what is in `my-project.cabal` (some comments and fields omitted for concision):
```haskell
cabal-version:      3.0
-- ...
common warnings
    ghc-options: -Wall
executable my-project
    import:           warnings
    main-is:          Main.hs

    -- Modules included in this executable, other than Main.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:

    -- Other library packages from which modules are imported.
    build-depends:    base ^>=4.17.2.1

    -- Directories containing source files.
    hs-source-dirs:   app

    -- Base language which the package is written in.
    default-language: GHC2021
```

The `executable my-project` clause describes some of the specifications of our project. In particular, the `build-depends` field describes any external dependencies we wish to include. These dependencies can be automatically pulled from Hackage by `cabal`, as long as we specify the name, and optionally the version, of the package. For example, we want the `Control.Monad.Trans.Maybe` module in `transformers` library. Hence, to include the `transformers` library to have access to monad transformers, just include `transformers` in `build-depends`.

```haskell
-- ...
executable my-project
    -- ...
    -- Other library packages from which modules are imported.
    build-depends:    base ^>=4.17.2.1
                    , transformers
    -- ...
```

Then, run `cabal install` to install all our dependencies!

```output
> cabal install
/path/to/my-project-0.1.0.0.tar.gz
Resolving dependencies...
Symlinking 'my-project' to '/path/to/.local/bin/my-project'
```

And that's all! Just like that, we now have access to `transformers` functions, data types, classes and methods! 

### Writing the Program
Let us try creating a simple executable program in our project. First, we create our simple graph library. Right now, our project directory looks like this:

```output
my-project/
├─ my-project.cabal
├─ app/
│   └─ Main.hs
└─ ...
```
Let us create a simple graph library by creating a file `my-project/app/Data/Graph.hs`, therefore our directory structure becomes:
```output
my-project/
├─ my-project.cabal
├─ app/
│   ├─ Main.hs
│   └─ Data/
│        └─ Graph.hs
└─ ...
```
This creates a new module called `Data.Graph`. We must include this in our `cabal` file so that `cabal` knows to compile it as well. Head back to `my-project.cabal`, and include `Data.Graph` in the `other-modules` field:

```haskell
-- ...
executable my-project
    -- ...
    -- Modules included in this executable, other than Main.
    other-modules:    Data.Graph
    -- ...
```

Now, open `Graph.hs` and write some code! In particular:
1. Declare the name of the module. In this case, the module is called `Data.Graph` because it is in the `Data` directory and the file name is `Graph.hs`.
2. Import the `Control.Monad.Trans.Maybe` module to have access to `MaybeT`, and the `Control.Monad.Trans.Reader` monad to have access to the `Reader` monad.
3. Define our `dfs` function.
```haskell
module Data.Graph where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Graph = [(Node, Node)]
type Node = Int

type GraphProcessor = MaybeT (Reader Graph) Int

dfs :: Node -> Node -> Graph -> Maybe Int
dfs src dst = runReader $ runMaybeT (aux src []) where
    aux :: Node -> [Node] -> GraphProcessor
    aux current visited 
      | arrived = return 0
      | alreadyVisited = MaybeT $ return Nothing
      | otherwise  = do
          n <- MaybeT $ reader $ lookup current 
          (+1) <$> aux n (current : visited)
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```
Note that our `Reader` monad shown in the previous chapter is quite different to the one defined in `transformers`. In fact, `Reader env a` is actually defined as `ReaderT env Identity a`. This is because it is generally quite uncommon to use the `Reader` monad by itself, since what it represents is just a plain function. The `ReaderT` monad transformer is defined as such:
```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
type Reader env a = ReaderT env Identity a
```
And the `Identity` monad is completely uninteresting:
```haskell
newtype Identity a = Identity { runIdentity :: a }
```
As such, the `transformers` library exposes some helper functions to make working with the plain `Reader` monad easier; for example, the `runReader` function extracts the enclosed function from a `ReaderT`, and the `reader` function transforms a function into a `ReaderT`.

We are done with our `Graph` library. Now, open `app/Main.hs` and write the following to see our `dfs` function in action (note that `print` is defined as `putStrLn . show`)!

```haskell
module Main where

import Data.Graph

myGraph :: Graph
myGraph = [(1, 2), (2, 3), (3, 1), (4, 5)]

main :: IO ()
main = do
  print $ dfs 1 2 myGraph
  print $ dfs 1 5 myGraph
```

We are done with developing our simple application! Compiling and running our program is simple with the help of build tools like `cabal`. In the terminal, just enter `cabal run` to compile the program (if changes have been made) and execute it!
```output info
> cabal run
Just 1
Nothing
```

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
