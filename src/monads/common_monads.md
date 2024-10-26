![Updated][update-shield]

# Commonly Used Monads
Thus far, we have looked at monads like `[]`, `Maybe` and `Either`. Recall that these monads describe the following notions of computation:
- `[]`: nondeterminism
- `Maybe`: potentially empty computation
- `Either a`: potentially failing computation

However, there are many more monads that you will frequently encounter, and in fact, many libraries (even in other programming languages) expose classes or data types that work as monads. Most of these monads involve one or both of the following notions of computation:
1. Reading from state
2. Writing to, or editing state

In fact, side effects can also be seen as reading from and writing to state. In this section, we shall describe some commonly used monads that implement these ideas.

## Reader
A very common pattern of computation is _reading from state_, i.e. performing computations based on some environment. For example, we may have a local store of users in an application, from which we retrieve some user information and do stuff with it. Typically, this is represented by a plain function of type `env -> a`, where `env` is the environment to read from, and `a` is the type of the result that depends on the environment. For example, we can determine if two nodes are connected in a graph by using depth-first search&mdash;however, connectivity of two nodes depends on the graph, where two nodes might be connected in one graph, but not in another. Therefore, the result of a depth-first search depends on the graph. However, depth-first search requires us to look up the neighbours of a node so that we can recursively search them, thereby also depending on the graph. As such, we want some way to compose two functions that receive a graph (monadically).


In general, we can let any term of type `env -> a` be seen as a term of type `a` that depends on an environment `env`. In other words, the type `env -> ?` describes the notion of computation of something depending on an environment. And as it turns out, for any environment type `env`, the partially applied type `(->) env` i.e. `env -> a` for all `a` is a `Monad`!

```haskell
instance Functor ((->) env) where
    fmap :: (a -> b) -> (env -> a) -> (env -> b)
    fmap f x = f . x

instance Applicative ((->) env) where
    pure :: a -> (env -> a)
    pure = const
    (<*>) :: (env -> (a -> b)) -> (env -> a) -> env -> b
    (<*>) f g x = f x (g x)

instance Monad ((->) env) where
    return :: a -> (env -> a)
    return = pure
    (>>=) :: (env -> a) -> (a -> (env -> b)) -> env -> b
    (>>=) m f x = f (m x) x
```

The definition of `fmap` is incredibly straightforward, essentially just doing plain function composition. The definition of `pure` is just `const`, where `const` is defined to be `const x = \_ -> x`, i.e. `pure` receives some value and produces a function that ignores the environment and produces that value. `<*>` takes two functions `f` and `g` and performs applicative application by applying each of them to the same environment `x`. Most notably, `<*>` applies the same environment _unmodified_ to both functions. Finally, `>>=` operates pretty similar to `<*>` except with some changes to how the functions are applied.

For clarity, let's define a type alias `Reader env a` which means that it is a type that reads an environment of type `env` and returns a result of type `a`:
```haskell
type Reader = (->)
```
Then, let's try to implement depth-first search with the `Reader` monad. First, we define some additional types, like the graph, which for us, has nodes as integers, and is represented using an adjacency list:
```haskell
type Node = Int
type Graph = [(Node, [Node])]
```
Next, we define a function `getNeighbours` which gets the nodes that are adjacent to a node in the graph:
```haskell
getNeighbours :: Node -> Reader Graph [Node]
getNeighbours x = do
    neighbours <- lookup x
    return $ concat neighbours
```
Notice that our `getNeighbours` function does not refer to the graph at all! We can just use `do` notation, and Haskell knows how to compose these computations!

Using `getNeighbours`, we can now define `dfs` which performs a depth-first search via recursion:
```haskell
dfs :: Node -> Node -> Reader Graph Bool
dfs src dst = aux [] src where
  aux :: [Node] -> Node -> Reader Graph Bool
  aux visited current
    | arrived         = return True
    | alreadyVisited  = return False
    | otherwise       = do
      neighbours <- getNeighbours current
      ls <- mapM (aux (current : visited)) neighbours
      return $ or ls
      where arrived = current == dst
            alreadyVisited = current `elem` visited
```
Let us learn how this works. Within the `dfs` function we define an auxiliary function that has a `visited` parameter. This is so that a user using the `dfs` function will not have to pass in the empty list as our `visited` "set". The `aux` function is where the main logic of the function is written. The first two cases are straightforward: (1) if we have arrived at the destination then we return `True`, and (2) if we have already visited the current node then we return `False`. If both (1) and (2) are not met, then we must continue searching the graph. We first get the neighbours of the current node using the `getNeighbours` function, giving us `neighbours`, which are the neighbours of the current node. Then, we recursively map `aux` (thereby recursively performing `dfs`) over all the neighbours. However, since `aux` is a monadic operation, we use `mapM` to map over the neighbours, giving us a list of results. We finally just check whether any of the nodes give us a positive result using the `or` function, corresponding to the `any` function in Python. Note one again that our `dfs` function makes no mention of the map at all, and we do not even need to pass the map into `getNeighbours`! The `Reader` monad automatically passes the same environment into all the other `Reader` terms that receive the environment. 

Using the `dfs` function is very simple. Since the `Reader` monad is actually just a function that receives an environment and produces output, to use a `Reader` term, we can just pass in the environment we want!

```haskell
ghci> my_map = [(1, [2, 3])
              , (2, [1])
              , (3, [1, 4])
              , (4, [3])
              , (5, [6])
              , (6, [5])]
ghci> dfs 5 6 my_map
True
ghci> dfs 5 2 my_map
False
ghci> dfs 1 2 [] -- empty map
False
```

Finally, note that we can retrieve the environment directly within the `Reader` monad by just using the identity function `id`!

```haskell
ask :: Reader env env
ask = id

getNeighbours :: Node -> Reader Graph [Node]
getNeighbours x = do
    my_graph <- ask -- gets the graph directly
    let neighbours = lookup x my_graph
    return $ concat neighbours
```

## Writer
The dual of a `Reader` is a `Writer`. In other words, instead of reading from some state or environment, the `Writer` monad has state that it writes to. The simplest example of this is logging. When writing an application, some (perhaps most) operations should be logged, so that we developers have usage information, crash dumps and so on, which can be later analysed. 

In general, we can let any term of type `(log, a)` be seen as a type `a` that also has a log `log`. And as it turns out, for any log type `log`, the partially applied type `(log,)`, i.e. `(log, a)` for all `a` is a `Monad`!

```haskell
instance Functor (log,) where
    fmap :: (a -> b) -> (log, a) -> (log, b)
    fmap f (log, a) = (log, f a)

instance Monoid log => Applicative (log,) where
    pure :: a -> (log, a)
    pure = (mempty,)
    (<*>) :: (log, a -> b) -> (log, a) -> (log, b)
    (<*>) (log1, f) (log2, x) = (log1 `mappend` log2, f x)

instance Monad (log,) where
    return :: a -> (log, a)
    return = pure
    (>>=) :: (log, a) -> (a -> (log, b)) -> (log, b)
    (log, a) >>= f = let (log2, b) = f a
                     in  (log1 `mappend` log2, b)
```
Let's carefully observe what the instances say. The `Functor` instance is straightforward&mdash;it applies the mapping function onto the second element of the tuple. The `Applicative` and `Monad` instances are more interesting. Importantly, just like the definition of the `Applicative` instance for `Validation`, the two logs are to be combined via an associative binary operation `<>`, which in this case is `mappend`. In most occasions, `mappend` is the same as `<>`. However, applicatives must also have a `pure` operation. In the case of `Either` and `Validation`, `pure` just gives a `Right` or `Success`, therefore not requiring any `log`. However, in a tuple, we need some "empty" `log` to add to the element to wrap in the tuple.

Thus, the `log` not only must have an associative binary operation, it needs some "empty" term that acts as the identity of the binary operation:
\\[E\oplus\textit{empty}=\textit{empty}\oplus E=E\\]
\\[E_1\oplus(E_2\oplus E_3)=(E_1\oplus E_2)\oplus E_3\\]

This is known as a `Monoid`, which is an extension of `Semigroup`!
```haskell
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
```
Typically, `mappend` is defined as `<>`.

Recall that `[a]` with concatenation is a `Semigroup`. In fact, `[a]` is also a `Monoid`, where `mempty` is the empty list!

```
ls ++ [] = [] ++ ls = ls
x ++ (y ++ z) = (x ++ y) ++ z
```

Therefore, as long as `a` is a `Monoid`, then `(a, b)` is a monad!

Lastly, just like how `Reader`s have an `ask` function which obtains the environment, `Writer`s have a `write` function which writes a message to your log&mdash;the definition of `write` makes this self-explanatory.

```haskell
write :: w -> (w, ())
write = (,())
```

Let us see this monad in action. Just like with `Validation`, we are going to let `[String]` be our log.

```haskell
type Writer = (,)
type Log = [String]
```
Then, we write an example simple function that adds a log message:

```haskell
loggedAdd :: Int -> Int -> Writer Log Int
loggedAdd x y = do
    let z = x + y
    write [show x ++ " + " ++ show y ++ " = " ++ show z]
    return z
```

Composing these functions is, once again, incredibly straightforward with `do` notation!

```haskell
loggedSum :: [Int] -> Writer Log Int
loggedSum [] = return 0
loggedSum (x:xs) = do
    sum' <- loggedSum xs
    loggedAdd x sum'
```

With this, the `loggedSum` function receives a list of integers and returns a pair containing the steps it took to arrive at the sum, and the sum itself:
```haskell
ghci> y = loggedSum [1, 2, 3]
ghci> snd y
6
ghci> fst y
["3 + 0 = 3","2 + 3 = 5","1 + 5 = 6"]
```

## State
However, many times, we will also want to compose functions that do _both_ reading from, and writing to or modifying state. In essence, it is somewhat a combination of the `Reader` and `Writer` monads we have seen. One example is pseudorandom number generation. A pseudorandom number generator receives a seed, and produces a random number and the next seed, which can then be used to generate more random numbers. The type signature of a pseudorandom number generation function would be something of the form:
```haskell
randomInt :: Seed -> (Int, Seed)
```
This pattern extends far beyond random number generation, and can be used to encapsulate the idea of a stateful transformation. For this, let us define a type called `State`:
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

Notice the `newtype` declaration. A `newtype` declaration is basically a `data` declaration, except that it must have exactly one constructor with exactly one field. In other words, a `newtype` declaration is a wrapper over a single type, in our case, the type `s -> (a, s)`. `newtype`s only differ from their wrapped types while programming and during type checking, but have no operational differences&mdash;after compilation, `newtype`s are represented exactly as the type they wrap, thereby introducing no additional overhead. However, `newtype` declarations also behave like `data` declarations, which allow us to create a new type from the types they wrap, allowing us to give new behaviours to the new type.

With this in mind, let us define the `Monad` instance for our `State` monad:

```haskell
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State f') = State $ 
        \s -> let (a, s') = f' s
              in  (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State (x,)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State f) (State x) = State $ 
        \s -> let (f', s') = f s
                  (x', s'') = x s'
              in  (f' x', s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= m = State $ 
        \s -> let (a, s') = f s
                  State f' = m a
              in  f' s'
```

The instance definitions are tedious to define. Furthermore, nothing worthy of note is defined&mdash;the methods implement straightforward function composition. However, it is these methods that allow us to compose stateful computation elegantly!

Finally, just like `ask` for `Reader`s and `write` for `Writer`s, we have `get` and `put` to retrieve and update the state of the monad accordingly, and an additional `modify` function which modifies the state:

```haskell
put :: s -> State s ()
put s = State $ const ((), s)

get :: State s s 
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do s <- get
              put (f s)
```

Let's try this with an example. Famously, computing the fibonacci numbers in a naive recursive manner is incredibly slow. Instead, by employing memoization, we can take the time complexity of said function from \\(O(2^n)\\) down to \\(O(n)\\). Memoization requires retrieving and updating state, making it an ideal candidate for using the `State` monad!

We first define our state to be a table storing inputs and outputs of the function. Then, writing the fibonacci function is straightforward. Note the use of `Integer` instead of `Int` so that we do not have integer overflow issues when computing large fibonacci numbers:
```haskell
type Memo = [(Integer, Integer)]
getMemoized :: Integer -> State Memo (Maybe Integer)
getMemoized n = lookup n <$> get

fib :: Integer -> Integer
fib n = fst $ runState (aux n) [] where
  aux :: Integer -> State Memo Integer
  aux 0 = return 0
  aux 1 = return 1
  aux n = do 
    x <- getMemoized n
    case x of
        Just y -> return y
        Nothing -> do
            r1 <- aux (n - 1)
            r2 <- aux (n - 2)
            let r = r1 + r2
            modify ((n, r) :)
            return r
```
The `getMemoized` function essentially just performs a lookup of the memoized input from the state. Then, the `fib` function defines an auxiliary function `aux` like before, which contains the main logic describing the computation of the fibonacci numbers. In particular, the `aux` function returns `State Memo Integer`. As such, to access the underlying state processing function produced by `aux n`, we must use the `runState` accessor function as defined in the `newtype` declaration for `State`. `runState (aux n)` gives us a function `Memo -> (Integer, Memo)`, and thus passing in the empty memo (`runState (aux n) []`) gives us the result. The result is a pair `(Integer, Memo)`, and since we do not need the memo after the result has been computed, we just discard it and return it from `fib`.

The `aux` function is similarly straightforward, with the usual two base cases. In the recursive case `aux n`, we first attempt to retrieve any memoized result using the `getMemoized` function. If the result has already been computed (`Just y`), then we return the memoized result directly. Otherwise, we recursively compute `aux (n - 1)` and `aux (n - 2)`. Importantly, `aux (n - 1)` will perform updates to the state (the memo), which is then passed along automatically (via monadic bind) to the call to `aux (n - 2)`, eliminating the exponential time complexity. Once `r1` and `r2` have been computed, the final result is `r`. Of course, we add the entry `n -> r` into the memo, and we can do so using the `modify` function, where `modify ((n, r) :)` prepends the pair `(n, r)` onto the memo. Of course, we finally return `r` after all of the above has been completed.

The result of this is polynomial-time `fib` function that can comfortably compute large fibonacci numbers:


```haskell
ghci> fib 1
1
ghci> fib 5
5
ghci> fib 10
55
ghci> fib 20
6765
ghci> fib 100
354224848179261915075
ghci> fib 200
280571172992510140037611932413038677189525
```

## I/O
Until now, we still have no idea how Haskell performs simple side effects like reading user input or printing to the console. In fact, nothing we have discussed so far involves side effects, because Haskell is a purely functional programming language, and all functions are pure. One of the key innovations of monads is that it allows a purely functional programming language like Haskell to produce side effects... but how?

Typically, a function that produces side effects is a regular function, except that it will also cause some additional effects on the side. One example is the `print` function in Python, which has the following type signature:
```python
def print(x: object) -> NoneType: # prints to the console
    # ...
```
However, notice that the `State` monad is somewhat similar. A term of `State s a` wraps a function `s -> (a, s)`; it is a pure function that is meant to compute a term of type `a`. However, it has the additional effect of depending on some state of type `s`, and will also produce some new state also of type `s`. Therefore, `State s a` can be seen as an impure function/term of type `a`, with the side effect of altering state.

What if the state `s` was actually the real world itself? In essence, the function `RealWorld -> (a, RealWorld)` is a function that receives the real world (as in, literally the world), and produces some term `a` and a new state of the world? In this view, a function that prints to the console receives the current state of the world and computes nothing (just like how `print` in Python returns `None`), and also produces the new state of the world where text has been printed to the console. Then, `input` in Python can be seen as a function that receives a state of the world containing user input, and produces the value entered by the user, retaining the current state of the world! These functions can thus actually be seen as _pure functions_, as long as we view the real world as a term in our programming language! In essence:

> The `IO` monad is the `State` monad where the state is the real world.

This is how Haskell, a purely functional programming language, performs I/O, a side effect. In fact, our characterization of `IO` is not merely an analogy, but is exactly how `IO` is represented in Haskell:

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

As such, after learning how the `State` monad works, performing I/O in Haskell should now be straightforward, especially with `do` notation. Let us finally, after five chapters, write a "Hello World" program.

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```
The `putStrLn` function has type `String -> IO ()`. It essentially receives a string to print, and alters the state of the world by adding the string to the console.

Importantly, every Haskell program can be seen as the `main` function, which has type `IO ()`. Recall that `IO` is just the `State` monad, which wraps a function that receives the state of the real world at function application, and produces a new state of the world and some other pure computation. In essence, the `main` function therefore has type `State# RealWorld -> (# State# RealWorld, () #)`. Therefore, we can see, roughly, that when a Haskell program is run, the current state of the world is passed into `main`, giving us a new state of the world where the program has completed execution! 

Just like the `State` monad, we can compose `IO` operations monadically with `do` notation. For example, the `getLine` function has type `IO String`, similar to `input` in Python except it does not receive and print a prompt. Thus, we can write a program that reads the name of a user and says hello to that user like so:

```haskell
-- Main.hs
main :: IO ()
main = do
    name <- getLine
    putStrLn $ "Hello " ++ name ++ "!"
```
Now, instead of loading the program with GHCi, we can _compile_ this program with GHC into an executable!

```
ghc Main.hs
```
When we run the program, the program waits for us to enter a name, then says hello to us!

```output info
Yong Qi
Hello Yong Qi!
```

Other `IO` operations can be found in Haskell's Prelude, and these should be relatively straightforward to understand.

[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
