### ReaderT IO Design Pattern

#### Evolution of an Application

_Dependency injection_ is the problem of providing data that is only known at runtime to the various layers of an application. It is a common problem in application architecture in any language. Haskell is notable in this respect because it is a language that forces you to solve dependency injection. `ReaderT` provides a relatively simple solution.

Consider this example. Suppose some parts of your program require a database connection and some other parts of your program require a handle to a UNIX socket. Neither the database connection nor the socket are available when you write your program, since they can only be created when the application boots. This introduces a bit of a paradox. You're writing your program well before this data is available, so how do your refer to it in your program.

An obvious solution is global variables, as in the below snippet.

**Exercise:**
Hoogle functions you're unfamiliar with in the snippet below order to see their signatures!
Write their signatures down in a separate text document, so you can refer to it as you read.
DO NOT GO TOO DEEP; just get the signatures of the things used here, and go no further.
It's okay if you don't understand every detail; you just want the signatures.

```haskell
module RuntimeDependencies (conn, sock, putLog, awaitLog) where

-- Globally-accessible database connection
conn :: Connection
conn = do
  connStr <- getEnv "CONN_STR"
  connection <- connectPostgreSQL $ pack connStr
  pure connection

-- Globally-accessible UNIX socket
sock :: Socket
sock = do
  sockAddr <- getEnv "SOCK_ADDR"
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix sockAddr
  pure s

-- Unexported, but needed in the bodies of `putLog` and `awaitLog`
logQueue :: TBQueue Text
logQueue = do
  q <- atomically $ newTBQueue 100
  let loop = do
        nextMsg <- atomically $ readTBQueue q
        T.hPutStrLn stderr nextMsg
        hFlush stderr
        loop
  void $ forkIO loop
  pure q

-- Put a log message on the queue.
-- Thread-safe: concurrent use can't corrupt the queue.
putLog :: Text -> IO ()
putLog = atomically . writeTBQueue logQueue

-- Block the current thread until the log queue is empty, giving the logger a chance to catch up.
-- Reuse-safe: both `putLog` and `awaitLog` can be safely reused after using `awaitLog`.
-- Thread-safe: concurrent use can't corrupt the queue.
awaitLog :: IO ()
awaitLog = atomically (tryPeekTBQueue logQueue) >>= ($ awaitLog) . when . not . null
-- Haskell code reads from left to right.
--         ^ Atomically
--                    ^ try to peek at logQueue,
--                                              ^ and with it
--                                                  ^ apply awaitLog
--                                                                 ^ when not null.
```

This module will be at the root of your application module tree, since the functions in other modules depend on these globals in order to do their jobs.

While there are admittedly downsides to this global variables approach, this is the quick and dirty way to get the job done in most other languages. But in Haskell, this won't work. Take a moment to think about why.

Hopefully you see the issue. The types I gave are wrong. `conn` isn't a `Connection`. `conn` is an `IO Connection`. Likewise `sock` is an `IO Socket`, and `logQueue` is an `IO (TBQueue String)`. Can we fix our problem by giving them correct type signatures? (We'll need to modifying `putLog` and `awaitLog` where needed.)

```haskell
module RuntimeDependencies (conn, sock, putLog, awaitLog) where

-- Globally-accessible database connection
conn :: IO Connection
conn = do
  connStr <- getEnv "CONN_STR"
  connection <- connectPostgreSQL $ pack connStr
  pure connection

-- Globally-accessible UNIX socket
sock :: IO Socket
sock = do
  sockAddr <- getEnv "SOCK_ADDR"
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix sockAddr
  pure s

-- Unexported, but needed in the bodies of `putLog` and `awaitLog`
logQueue :: IO (TBQueue Text)
logQueue = do
  q <- atomically $ newTBQueue 100
  let loop = do
        nextMsg <- atomically $ readTBQueue q
        T.hPutStrLn stderr nextMsg
        hFlush stderr
        loop
  void $ forkIO loop
  pure q

-- Put a log message on the queue.
-- Thread-safe: concurrent use can't corrupt the queue.
putLog :: Text -> IO ()
putLog msg = do
  q <- logQueue
  atomically $ writeTBQueue q msg

-- Block the current thread until the log queue is empty, giving the logger a chance to catch up.
-- Reuse-safe: both `putLog` and `awaitLog` can be safely reused after using `awaitLog`.
-- Thread-safe: concurrent use can't corrupt the queue.
awaitLog :: IO ()
awaitLog = do
  q <- logQueue
  atomically (tryPeekTBQueue q) >>= ($ awaitLog) . when . not . null
```

At least this type checks now, but it doesn't solve our problem. Take a moment to think about why, and think carefully about what a value of type `IO Connection` or `IO Socket` represents in Haskell.

Did you figure it out? This version of `conn` will create a _new_ database connection everywhere it's used. That's not what we want! Similarly, the new `sock` will try to connect everywhere it's used. And our log messages are getting garbled because every use of `putLog` is creating a new worker thread that's competing over `stderr`. Not to mentions, all those extra queues we're creating and only using once are cluttering up our heap because they can't be garbage collected. That's _really_ not what we want!

A common misconception people have about Haskell is that an `IO Connection` represents an "impure `Connection`" (whatever that means). In fact, a value of type `IO Connection` represents a _program_ that produces a `Connection`. Likewise, an `IO Socket` is a program that produces a `Socket`. In some contexts, "produces" could mean it gives you a pointer to a pre-established `Connection`, but it's clear from the code above that `conn`, `sock`, and `putLog` aren't doing that. This code isn't sharing pre-established resources. We want our app to establish a single connection, open a single socket, and create a single log queue at boot, and we want to share these resources with the rest of our code. This creates a chicken-and-egg problem.

In general, the solution to these circularity problems is always the same: abstraction (by which I mean functions parameters, the ). A straightforward way to solve this problem is to re-write your app's business logic so that functions get passed a `Connection` as an argument, and similarly for `Socket` and the `putLog` and `awaitLog` functions. We would create our `Connection`, our `Socket`, and our `putLog` and `awaitLog` functions in our `main`. Then we'd pipe these resources down through our call graph so that they can get to the functions that depend on them.

```haskell
module Main (main) where
import BusinessLogic (entryPoint)

makeConn :: IO Connection
makeConn = do
  connStr <- getEnv "CONN_STR"
  connection <- connectPostgreSQL connStr
  pure connection

makeSock :: IO Socket
makeSock = do
  sockAddr <- getEnv "SOCK_ADDR"
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix sockAddr)
  pure s

makeLog :: IO (Text -> IO (), IO ())
makeLog = do
  q <- atomically $ newTBQueue 100
  let loop = do
        nextMsg <- atomically $ readTBQueue q
        T.hPutStrLn stderr nextMsg
        hFlush stderr
        loop
      put = atomically . writeTBQueue q
      await = atomically (tryPeekTBQueue q) >>= ($ await) . when . not . null
  void $ forkIO loop
  pure (put, await)

main :: IO ()
main = do
  conn <- makeConn
  sock <- makeSock
  (putLog, awaitLog) <- makeLog
  entryPoint conn sock putLog awaitLog
  close conn
  shutdown ShutdownBoth sock
  awaitLog
```

Then, (don't worry about the names, I'm just making them up) code that used to look like this

```haskell
module BusinessLogic (entryPoint) where

entryPoint :: IO ()
entryPoint = do
  x <- foo
  y <- bar x
  z <- baz x
  awaitLog
  qux y z
  awaitLog

data X
data Y
data Z
data W

foo :: IO X
foo = do
  let [this, that, wherefore, qed] = undefined
  those <- this that
  undefined those
  qed

bar :: C-> IO (IO Y)
bar x = undefined "..."

baz :: X -> IO Z
baz x = undefined "..."

qux :: IO Y -> Z -> IO W
qux y z = undefined "..."
```

now looks like this.

```haskell
module BusinessLogic (entryPoint) where

entryPoint :: Connection -> Socket -> (Text -> IO ()) -> IO () -> IO ()
entryPoint conn sock putLog awaitLog = do
  x <- foo conn sock putLog awaitLog
  y <- bar conn sock putLog awaitLog x
  z <- baz conn sock putLog awaitLog x
  awaitLog
  qux conn sock putLog awaitLog y z
  awaitLog

data X
data Y
data Z
data W

foo :: Connection -> Socket -> (Text -> IO ()) -> IO () -> IO X
foo conn sock putLog awaitLog = do
  let [this, that, wherefore, qed] = undefined
  those <- this conn sock putLog awaitLog that
  wherefore conn sock putLog awaitLog those
  qed conn sock putLog awaitLog

bar :: Connection -> Socket -> (Text -> IO ()) -> IO () -> X -> IO (IO Y)
bar conn sock putLog awaitLog x = undefined "..."

baz :: Connection -> Socket -> (Text -> IO ()) -> IO () -> X -> IO Z
baz conn sock putLog awaitLog x = undefined "..."

qux :: Connection -> Socket -> (Text -> IO ()) -> IO () -> IO Y -> Z -> IO W
qux conn sock putLog awaitLog y z = undefined "..."
```

This gets very annoying very quickly. Most of these functions aren't even going to do anything with the shared resources other than pass them along to the functions they call. This makes our code verbose, which makes it hard to read, hard to understand, and hard to change.

One low-hanging-fruit improvement we can make is to define a data structure to hold all of these shared resources. That way we only have to pass around one extra argument instead of four.

```haskell
module BusinessLogic (AppCtx (..), entryPoint) where

data AppCtx = AppCtx
  { conn :: Connection
  , sock :: Socket
  , putLog :: Text -> IO ()
  , awaitLog :: IO ()
  }

entryPoint :: AppCtx -> IO ()
entryPoint ctx = do
  x <- foo ctx
  y <- bar ctx x
  z <- baz ctx x
  awaitLog ctx
  qux ctx y z
  awaitLog ctx

data X
data Y
data Z
data W

foo :: AppCtx -> IO X
foo ctx = do
  let [this, that, wherefore, qed] = undefined
  those <- this ctx that
  wherefore ctx those
  qed ctx

bar :: AppCtx -> X -> IO (IO Y)
bar ctx x = undefined "..."

baz :: AppCtx -> X -> IO Z
baz ctx x = undefined "..."

qux :: AppCtx -> IO Y -> Z -> IO W
qux ctx y z = undefined "..."
```

Still a nuisance, albeit a much smaller nuisance. But this is Haskell, where we can do better.

_Note:_ We're about to build up a ton of extra machinery in order to grant us a little extra convenience. In order to justify this, our application needs to be big enough for the convenience we gain, amortized over the whole application, to outweigh the cost of building up the extra machinery Extra convenience almost always means extra machinery. Whether or not the extra machinery is worth the effort depends mostly on the size of your app, the complexity of the machinery, and the relative convenience the machinery affords. For small apps (on the order of 2000 or fewer lines), the refactor we just demonstrated in the code block above works fine, and the methods we will develop in the rest of this essay are probably not worth the effort. But passed about 5000 lines or so, the methods shown below are almost certainly going to pay dividends. Between 2000 and 5000 lines is a toss up, but it's better to err on the side of assuming your application will grow. That said, always weigh the costs and the benefits of various architectural patterns to avoid over-engineering your app.

A little philosophy: one way to think about what monads do in Haskell is that they add features to the language that the language designers and compiler engineers didn't anticipate and didn't provide as built-ins. The feature we want in this case is for a certain `Connection`, a certain `Socket`, and certain log-related utilities to always be in scope. That's a pretty neat language feature, but it's so bespoke to our app that it's no wonder the language designers didn't anticipate it! With Haskell, we can change that. We can design a monad for our application that provides this language feature. (More accurately, a monad that _models,_ as data, programs written in a language that has this feature.)

```haskell
module App (AppCtx (..), App, appCtx, conn, sock, putLog, awaitLog, appIO, runApp) where

data AppCtx = AppCtx
  { appConn :: Connection
  , appSock :: Socket
  , appPutLog :: Text -> IO ()
  , appAwaitLog :: IO ()
  }

-- A data structure that provides the missing language features!
newtype App a = App (AppCtx -> IO a)

instance Functor App where
  fmap = undefined "..." -- boilerplate

instance Applicative App where
  pure = undefined "..." -- boilerplate
  (<*>) = undefined "..." -- boilerplate

instance Monad App where
  (>>=) = undefined "..." -- boilerplate

-- A new language feature `App` provides is the ability to grab the application context,
-- basically out of thin air.
appCtx :: App AppCtx
appCtx = App (\c -> pure c)

-- Another one of the new language features `App` provides!
conn :: App Connection
conn = App $ \AppCtx {appConn} -> pure appConn

-- Another new language feature provided by `App`!
sock :: App Socket
sock = App $ \AppCtx {appSock} -> pure appSock

-- And another!
putLog :: Text -> App ()
putLog msg = App $ \AppCtx {appPutLog} -> appPutLog msg

-- And another!
awaitLog :: App ()
awaitLog = App $ \AppCtx {appAwaitLog} -> appAwaitLog

-- `appIO` will be explained in the paragraphs below.
appIO :: IO a -> App a
appIO io = App $ \_ -> io

-- `runApp` will be explained in the paragraphs below.
runApp :: AppCtx -> App a -> IO a
runApp ctx (App f) = f ctx
```

```haskell
module BusinessLogic (entryPoint) where
import App

entryPoint :: App ()
entryPoint = do
  x <- foo
  y <- bar x
  z <- baz x
  awaitLog
  qux y z
  awaitLog

data X
data Y
data Z
data W

foo :: App X
foo = do
  let [this, that, wherefore, qed] = undefined
  those <- this that
  wherefore those
  qed

bar :: X -> App (App Y)
bar x = undefined "..."

baz :: X -> App Z
baz x = undefined "..."

qux :: App Y -> Z -> App W
qux y z = undefined "..."
```

`App` now models a programming language that's specially designed for our application. Its features include all the features of Haskell plus the ability to reference a global `Connection`, a global `Socket`, and some global logging utilities. A value of type `App T` represent a program written in this new language, specifically a program that computes a value of type `T`. Conveniently for us, our business logic functions no longer need to take an `AppCtx` as argument; they can use the new language primitives `conn`, `sock`, `putLog`, and `awaitLog` to get the resources they depend on.

There are several points of contact between our new `App` language and the rest of Haskell, and it's important we understand them well.

`App` is _our_ thing. The Haskell code that other people write doesn't know anything about `App`. In order for us to user their code, we'll need a way to "lift" (a colloquial term, not a precise term) their Haskell code into our `App` language. First, `Applicative App` gives us `pure :: a -> App a`. Second, we have `appIO :: IO a -> App a`. Having `appIO` gives us a way to lift Haskell's built-in `IO` values and the `IO` values defined in third-party libraries. With `appIO`, `App` effectively _inherits_ all built-in and third-party `IO` operations. We'll use `appIO` throughout our application.

On the flip side, GHC doesn't know how to turn `App` values into executables. In order for GHC to be able to use our code, we need to have a way to put our program into a language that GHC natively understands. GHC knows how to turn `IO` values into executables. In fact, `IO` is the only language that GHC natively understands. That's why `main` has to have type `IO ()`. `runApp` is the contact point we'll use to convert programs written in our `App` language into programs written in Haskell's native `IO` language. We'll use `runApp` exactly once, in our `main`.

So, now, our `Main` module should look like this:

```haskell
module Main (main) where
import App (AppCtx (..), runApp)
import BusinessLogic (entryPoint)

makeConn :: IO Connection
makeConn = do
  connStr <- getEnv "CONN_STR"
  connection <- connectPostgreSQL $ pack connStr
  pure connection

makeSock :: IO Socket
makeSock = do
  sockAddr <- getEnv "SOCK_ADDR"
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix sockAddr)
  pure s

makeLog :: IO (Text -> IO (), IO ())
makeLog = do
  q <- atomically $ newTBQueue 100
  let loop = do
        nextMsg <- atomically $ readTBQueue q
        T.hPutStrLn stderr nextMsg
        hFlush stderr
        loop
      put = atomically . writeTBQueue q
      await = atomically (tryPeekTBQueue q) >>= ($ await) . when . not . null
  void $ forkIO loop
  pure (put, await)

main :: IO ()
main = do
  appConn <- makeConn
  appSock <- makeSock
  (appPutLog, appAwaitLog) <- makeLog
  runApp AppCtx {..} entryPoint
  close conn
  shutdown ShutdownBoth sock
  awaitLog
```

We did it! We did dependency injection for our whole application. If later we need to add another runtime dependency, all we do is add a field to `AppCtx`. Notably, we didn't refactor `makeConn`, `makeSock`, and `makeLog` into `App` operations. They need to remain `IO` operations, since they're used in our `main` during application bootstrapping.

#### `MonadReader c` and `ReaderT c m`: Lock and Key

There's at least one more unsatisfying thing, though. Writing `Functor App`, `Applicative App`, and `Monad App` instances are utterly formulaic. There's got to be a way to avoid that boilerplate. This is where `ReaderT` comes in.

**Exercise:**
Implement `Functor App`, `Applicative App`, and `Monad App` directly, without using `ReaderT`.

Here are the (significantly simplified) definitions of `ReaderT` and a few other related things.

```haskell
module Control.Monad.Reader where

newtype ReaderT c m a = ReaderT (c -> m a)

runReaderT :: ReaderT c m a -> (c -> m a)
runReaderT (ReaderT f) = f

instance Functor m => Functor (ReaderT c m) where
  fmap = undefined "..." -- boilerplate, exact same as from before

instance Applicative m => Applicative (ReaderT c m) where
  pure = undefined "..." -- boilerplate, exact same as from before
  (<*>) = undefined "..." -- boilerplate, exact same as from before

instance Monad m => Monad (ReaderT c m) where
  (>>=) = undefined "..." -- boilerplate, exact same as from before

...
```

We also define a corresponding class, `MonadReader` (presented here in a simplified form),

```haskell
... -- (Control.Monad.Reader continued)

class Monad m => MonadReader c m where
  ask :: m c

...
```

Conceptually, `MonadReader c` is "the class (i.e. collection, set, group) of all Haskell monads that provide access to a global value of type `c`." The `ask` function, specifically, is what provides this access. Notice that we can write an instance of `MonadReader AppCtx` for our `App` monad by defining `ask = appCtx`, so `App` can be a member of this class. The `MonadReader c` class is a useful concept because many of the monads we need to work with provide various other features besides access to a global `c` value, and `MonadReader c` allows us to to interact with all of these monads in a uniform way. In particular, we can write functions that work for any of these monads, polymorphically.

There's a neat thing about the relation between the `ReaderT c m` type constructor and the `MonadReader c` class. `ReaderT c m` is the _smallest possible_ member of `MonadReader c`. This is a bold claim, and we'll explain what it means in a bit, but first let's just make sure that `ReaderT c m` actually is a member of `MonadReader c`.

```haskell
... -- (Control.Monad.Reader continued)

instance Monad m => MonadReader c (ReaderT c m) where
  ask = ReaderT $ \c -> pure c -- it practically writes itself...

...
```

This instance says that if `m` is a monad then `ReaderT c m` is a member of `MonadReader c`. (Notably, the implementation of `pure` we're using on the right-hand side is the implementation for `m`, not the implementation for `ReaderT c m`.) This instance is the key to my earlier claim about the minimality of `ReaderT c m` in the class `MonadReader c`. Here's what minimality means in this context. If `m` is a member, any member, of `MonadReader c`, there will always be some monad `n` with the property that `ReaderT c n` has feature parity with `m`.

Think about that for a moment. Think about what it means for us, pragmatically.

It means that we can _build_ reader monads out of simpler pieces!

Say we're designing a monad `M` for our application that provides, among other sick features, access to a global value of type `C`. The minimality of `ReaderT c m` tells us that there's a way to build a suitable `M` out of simpler pieces. Namely, we can define `newtype M a = ReaderT C N a`. This ensures that `M` has the global `C` feature we want, and our design problem is simplified down to finding a monad `N` that provides the other features we want.

I hope your bullshit detectors are going off, because I just swept a _very important detail_ under the rug. Suppose we do come up with a monad `N` that has the remaining features we want. Why should we assume that those features of `N` will translate to features of `ReaderT C N`? You're right, we shouldn't assume it! We need to put our cursors where our mouths are. We need to know precisely which features of `m` transfer seamlessly to `ReaderT c m`, and we need to say how that's done, if we want the computer to do it for us.

In particular, we need to be more precise about what we mean when we say "a feature of `m`." For each conceptual feature set we're interested in, we can define a class. The class's associated functions represent the language features we want. That's is exactly what `MonadReader` does, when you think about it.

One of our favorite language features is the ability to make system calls and manipulate RAM, the feature provided by Haskell's built-in `IO` monad. In a moment we'll see `MonadIO`, the class of all monads that have this feature. `MonadIO` has an associated function called `liftIO`. We'll see an instance defining how `ReaderT c m` can surface this feature if `m` has it. Finally, `IO` has all the features of `IO` (clearly!), so we'll make sure GHC knows it by making an instance `MonadIO IO`.

```haskell
... -- (Control.Monad.Reader continued)

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO m => MonadIO (ReaderT c m) where
  liftIO io = ReaderT $ \_ -> liftIO io

instance MonadIO IO where
  liftIO = id
```

This is great, because it allows our `newtype M a = ReaderT C N a` to automatically inherit all of Haskell's `IO` features (both built-ins and third-party `IO` operations) whenever `N` does. In particular, we're all done if the only other features we're interested in is Haskell's `IO`: just use `IO` for `N`. Let's refactor our `App` to use this new machinery.

```haskell
module App (AppCtx (..), App, ask, conn, sock, putLog, awaitLog, liftIO, runApp) where
import Control.Monad.Reader

data AppCtx = AppCtx
  { appConn :: Connection
  , appSock :: Socket
  , appPutLog :: Text -> IO ()
  , appAwaitLog :: IO ()
  }

newtype App a = App (ReaderT AppCtx IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppCtx)

conn :: App Connection
conn = fmap appConn ask

sock :: App Socket
sock = fmap appSock ask

putLog :: Text -> App ()
putLog = fmap (liftIO . appPutLog) ask

awaitLog :: App ()
awaitLog = fmap (liftIO appAwaitLog) ask

runApp :: AppCtx -> App a -> IO a
runApp ctx (App f) = runReaderT f ctx
```

Practically no boilerplate! We could even get rid of `conn`, `sock`, `putLog` and `awaitLog` if we wanted, as they're basically one-liners at this point.

#### Capabilities Patterns

By exposing `ask :: App AppCtx` and `liftIO :: IO a -> App a`, `App` provides several unrelated features. In sufficiently-large applications, it could make sense to refine this set of features into several smaller components. Doing so adds a bit of boilerplate but also makes our functions more abstract. Making our functions more abstract decouples them, makes them easier to implement correctly, helps us locate and correct bugs, and can reduce cognitive load.

Looking at `conn`, `sock`, `putLog` and `awaitLog`, it looks like there are three logical domains: database access, socket access, and log access. We'll make a class for each one. Making a class for logging is pretty straightforward, as we're already encapsulating our message queue and exposing functions that manipulate it. Simply associate those functions to a class.

```haskell
class Monad m => AppLog m where
  putLog :: Text -> m ()
  awaitLog :: m ()

instance AppLog App where
  putLog msg = ask >>= \ctx -> liftIO $ appPutLog ctx msg
  awaitLog = ask >>= liftIO . appAwaitLog
```

Making classes for database access and socket access are not as straightforward. One approach is to hand the relevant resource over, `Reader` style.

```haskell
class Monad m => ReaderConnection m where
  askConn :: m Connection

instance ReaderConnection App where
  askConn = fmap appConn ask

class Monad m => ReaderSocket m where
  askSock :: m Socket

class ReaderSocket App where
  askSock = fmap appSock ask
```

This is a popular style due to its simplicity, though it does have some drawbacks. I tend to call this approach _resource juggling,_ as the caller is given explicit access to the resource and then manipulates it directly. As a result, this style doesn't offer any real encapsulation. An alternative approach is to mimic what we did for the logs and encapsulate these resources.

First ask yourself, _what would I want to do with a `Socket`/`Connection` if I had one?_ Then ask, _what would I want to *prevent* myself from accidentally doing with a `Socket`/`Connection`?_ For example, explicit access to a `Connection` means the caller can `close` the connection. This would be a pretty silly thing to do if you had the full context of the application in mind: you'd know that the `Connection` is shared and shouldn't be closed except in `main`. But what about new hires? They might (not-unreasonably) be under the impression that `askConn` gives them a _fresh_ connection, and that they _should_ close it when they're done. We can't prevent that if we hand them a `Connection`, but we can prevent that if we encapsulate the `Connection`.

Below are my answers to the questions of the above paragraph, in the form of a simplified API for each of `Socket` and `Connection`.

```haskell
-- Simplified Database.PostgreSQL.Simple `Connection` API
execute :: ToRow q => Connection -> Query -> q -> IO Int64
executeMany :: ToRow q => Connection -> Query -> [q] -> IO Int64
query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
returning :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> IO [r]
begin :: Connection -> IO ()
commit :: Connection -> IO ()
rollback :: Connection -> IO ()
```

```haskell
-- Simplified Network.Socket `Socket` API
listen :: Socket -> Int -> IO ()
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
touchSocket :: Socket -> IO ()
```

We'll define classes that expose these features to our application business logic. Here's the API I chose for our `Socket` access.

```haskell
class Monad m => AppSocket m where
  listen :: Int -> m ()
  recvBuf :: Ptr Word8 -> Int -> m Int
  sendBuf :: Ptr Word8 -> Int -> m Int
  touchSocket :: m ()

instance AppSocket App where
  listen n = sock >>= \s -> liftIO $ Network.Socket.listen s n
  recvBuff p n = sock >>= \s -> liftIO $ Network.Socket.recvBuff s p n
  sendBuf p n = sock >>= \s -> liftIO $ Network.Socket.recvBuff s p n
  touchSocket = sock >>= \s -> liftIO $ Network.Socket.touchSocket s
```

A little boilerplatey, but not too bad, and it does the job. The `IO` is abstracted by the type variable `m` and the `Socket` is encapsulated so we never have to worry about it again.

Notice that the `Connection` API includes `begin`, `commit`, and `rollback` for defining transactional logic. Defining transactions can be somewhat tricky. For example, what happens if you accidentally `begin` a transaction while in the middle of another one? What happens if you block an socket I/O while a transaction is open? Why not just make sure we never accidentally find out? We want to make it easy to do the right thing, so let's manage transactional logic once and for all in our class so that we get it right every single time. We'll do this by defining a type constructor, `DbTxn`.

```haskell
newtype Txn a = Txn (ReaderT Connection IO a)
  deriving newtype (Functor, Applicative, Monad)

execute :: ToRow q => Query -> q -> Txn Int64
execute qry q = Txn $ ReaderT $ \conn -> Database.PostgreSQL.Simple.execute conn qry q

executeMany :: ToRow q => Query -> [q] -> Txn Int64
executeMany qry qs = Txn $ ReaderT $ \conn -> Database.PostgreSQL.Simple.executeMany conn qry qs

query :: (ToRow q, FromRow r) => Query -> q -> Txn [r]
query qry q = Txn $ ReaderT $ \conn -> Database.PostgreSQL.Simple.query conn qry q

returning :: (ToRow q, FromRow r) => Query -> [q] -> Txn [r]
returning qry qs = Txn $ ReaderT $ \conn -> Database.PostgreSQL.Simple.returning conn qry qs

class Monad m => AppTxn m where
  runTxn :: Txn a -> m a

instance AppTxn App where
  runTxn (Txn (ReaderT f)) = do
    c <- conn
    liftIO $ do
      begin c
      result <- try $ f c
      -- For silly reasons that I don't want to spend time on right now,
      -- the IRL error-matching logic is going to end up being more complex.
      case result of
        Left err -> do
          rollback c
          throwIO err
        Right rows -> do
          commit c
          pure rows
```

First thing to notice here: `Txn` is not given `MonadIO` or `MonadReader Connection` instances. This is intentional. If `Txn` had a `MonadIO` instance, we'd be able to `liftIO . begin` right in the middle of an existing transaction. No bueno. And giving `Txn` a `MonadReader Connection` instance would defeat the encapsulation we're trying to achieve.

Second thing to notice: the `execute`, `executeMany`, `query`, and `returning` are not associated to the class `AppTxn`. Instead, they return concrete `Txn` values. `AppTxn m` merely defines a mechanism whereby a `Txn` value may be "executed in `m`" (a colloquial phrase). This should remind you of how `MonadIO m` defines `liftIO :: IO a -> m a`. The purpose behind designing `AppTxn` this way is to make sure that values of type `Txn` are build up as individual units, distinguishable from the rest of our application business logic. Consequently, we're prevented from interleaving the features that make up our larger `App` language (such as `liftIO`, `ask`, `putLog`, `awaitLog`, `listen`, `recvBuf`, `sendBuf`, and `touchSocket`) from the features that make up the `Txn` language (namely `execute`, `executeMany`, `query`, and `returning`). This is exactly what we want for database transaction. (E.g. since we'd be holding on to a database lock for the duration of a transaction, we don't want to start blocking on socket I/O.)

The style we chose for `AppLog` and `AppSocket` is complementary to the style we chose for `AppTxn`, and both styles are colloquially called "capabilities." The style we picked for `AppLog` and `AppSocket` if often called "MTL-style," but this name is historic and no longer makes sense. A newer name sometimes used (especially in other programming language communities) is "tagless final style," but I prefer the name "service-oriented style." Think of `AppLog` and `AppSocket` as describing a service available to our application, and they're seamlessly blended with the rest of the `App` language. I don't think there's a name yet for the style we picked for `AppTxn`, but good candidates would be "transformational style" (`runTxn` transforms `Txn` programs into `App` programs), "DSL-style" (the `Txn` language is a domain-specific language with features that are kept distinct from the features of the `App` language), "transactional style" (for reasons that are obvious in this example, but the name might not work so well for other instances of this pattern), or "interpreter style" (`runTxn` is an interpreter for the `Txn` language written in the `App` language).

With all this machinery in place, our business logic will end up looking something like this.

```haskell
module BusinessLogic (entryPoint) where
import AppLog
import AppSocket
import AppTxn

entryPoint :: (AppLog m, AppSocket m, AppTxn m) => m ()
entryPoint = do
  x <- foo
  y <- bar x
  z <- baz x
  awaitLog
  runTxn $ qux y z
  awaitLog

data X
data Y
data Z
data W

foo :: AppLog m => m X
foo = do
  let [this, that, wherefore, qed] = undefined
  those <- this that
  wherefore those
  qed

bar :: (AppSocket m, AppLog m) => X -> m (Txn Y)
bar x = undefined "..."

baz :: AppLog m => X -> m Z
baz x = undefined "..."

qux :: Txn Y -> Z -> Txn W
qux y z = undefined "..."
```

Notice we've eliminated all direct references to `App` and `AppCtx` from our business logic. This is great from a decoupling standpoint. We also have fine-grained information about what different functions can do, which is useful when we need to debug something.

Even though `entryPoint` doesn't mention `App`, we'll still need to use `runApp` in our `main` in order to concretize `entryPoint` by instantiating `entryPoint` with `m` equal to `App`.

```haskell
module Main (main) where
import App (AppCtx (..), runApp)
import BusinessLogic (entryPoint)

makeConn :: IO Connection
makeConn = do
  connStr <- getEnv "CONN_STR"
  connection <- connectPostgreSQL $ pack connStr
  pure connection

makeSock :: IO Socket
makeSock = do
  sockAddr <- getEnv "SOCK_ADDR"
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix sockAddr)
  pure s

makeLog :: IO (Text -> IO (), IO ())
makeLog = do
  q <- atomically $ newTBQueue 100
  let loop = do
        nextMsg <- atomically $ readTBQueue q
        T.hPutStrLn stderr nextMsg
        hFlush stderr
        loop
      put = atomically . writeTBQueue q
      await = atomically (tryPeekTBQueue q) >>= ($ await) . when . not . null
  void $ forkIO loop
  pure (put, await)

main :: IO ()
main = do
  appConn <- makeConn
  appSock <- makeSock
  (appPutLog, appAwaitLog) <- makeLog
  runApp AppCtx {..} entryPoint
  close conn
  shutdown ShutdownBoth sock
  awaitLog
```

To demonstrate how this pattern has decoupled our application, we can inspect a dependency graph. In this graph, blue text are our modules, and blue arrows are imports of internal modules. Red text are third-part Haskell packages, and red arrows are imports of _functions_ from a third-party package.

![New dependency graph](temp-new-deps-graph.png)

The first thing you notice is that the new dependency structure is shallow and wide, which is great for code maintenance. This first thing we notice is that `BusinessLogic` and `App` are completely independent of each other. The next thing we notice is that, while they do use types from third-party libraries, they don't depend on any functionality from third-party libraries. The only place we depend on such functionality is in our `Main`. This gives us a high-level of decoupling from our dependencies, since we don't have their functions permeating our business logic. We could even go further and excise all the third party types from `AppLog`, `AppSocket`, and `AppTxn` using various techniques, such as replacing them with type parameters or defining our own types and converting, though diminishing marginal gains is already starting to kick in. What he have so far is good enough. Besides, types in Haskell are basically an interface: they specify how functions fit together without containing any functionality of their own. We've effectively refactored our application so that it only uses the interfaces provided by third-party libraries, and we've isolated all the implementation-coupling to a few lines in `Main`.