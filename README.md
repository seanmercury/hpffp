# Haskell Programing from First Principal

## Chap 1 - All you need is Lambda. 

- The lambda in lambda calculus is the greek letter 𝜆 used to introduce, or abstract, arguments for binding in an expression.
- A lambda abstraction is an anonymous function or lambda term.
(𝜆𝑥.𝑥 + 1)
The head of the expression, 𝜆𝑥., abstracts out the term 𝑥 + 1. We can apply it to any 𝑥 and recompute different results for each 𝑥 to which we apply the lambda.

## Chap 2 - Hello Haskell.
- argument vs parameter
- An expression is a combination of symbols that conforms to syntactic rules and can be evaluated to some result.
- A value is an expression that cannot be reduced or evalu- ated any further. 
- A function is a mathematical object that can be applied to an argument in order to return a result
- Syntactic sugar is syntax within a programming language designed to make expressions easier to read and write

## Chap 3 - Strings
- String is represented by a linked list of Char values, aka [Char]
- A type or datatype is a classification of values or data.
- Concatenation is the joining together of sequences of val- ues.
  ```
  Prelude> "tacos" ++ " " ++ "rock"
     "tacos rock"
  ```

- scope is where a variable can be validly referred to by name in a program

## Chap 4 - Datatypes
- A tuple is an ordered grouping of values.
  - unit or () : zero tuple
- A type class is a set of operations defined with respect to a polymorphic type.
- Data constructors in Haskell provide a means of creating values that inhabit a given type.
  - data Pet = Cat | Dog Name
- Type constructors in Haskell are not values and can only be used in type signatures.
- Data declarations define new datatypes in Haskell. Data declarations always create a new type constructor but may or may not create new data constructors. Data declarations are how we refer to the entire definition that begins with the data keyword.
- A type alias is a way to refer to a type constructor or type constant by an alternate name, usually to communicate something more specific or for brevity:
  ``` 
  type Name = String
  ```
- Polymorphism in Haskell, specifically, is either parametric or constrained. 
- Parametric or ad-hoc polymorphic function:
  ```
  id :: a -> a
  id x = x

- Constrained or Boundedv Polymorphic
  ```
  isEqual :: Eq a => a -> a -> Bool 
  isEqual x y = x == y

## Chap 5 - Types
- Type inference is to infer principal types(most generic type that still type checks) from terms without requiring explicit type annotations.
- A type variable is a way to refer to an unspecified type or set of types in Haskell type signatures.
- A type class is a means of expressing faculties or interfaces that multiple datatypes may have in common. 
- Parametricity is the property that holds in the presence of parametric polymorphism. 
- A module is the unit of organization that the Haskell pro- gramming language uses to collect together declarations of values, functions, datatypes, type classes, and type class instances.

## Chap 6 - Type Classes
- Type class inheritance is when a type class has a superclass. This is a way of expressing that a type class requires another type class to be available for a given type before you can write an instance:
  ```
  class Num a => Fractional a where 
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a

- IO is the type for values whose evaluation bears the possi- bility of causing side effects, such as printing text, reading text input from the user, reading or writing to files, or connecting to remote computers. 
- An instance is the definition of how a type class should work for a given type.

See [Practice](../practice/typeCheck) for Chap 6 & 7

## Chap 7 - More Functional Patterns
- anonymous function
  ```
  triple x = x * 3
  (\x -> x * 3)

  with specific type
  (\x -> x * 3) :: Integer -> Integer
  (\x -> x * 3 :: Integer)

  ghci> (\x -> x * 3) 1

  mTh x y z = x * y * z
  mTh x y = \z -> x * y * z
  mTh x = \y -> \z -> x * y * z
  mTh = \x -> \y -> \z -> x * y * z

  ```
  
- pattern matching
  ```
  isItTwo :: Integer -> Bool
  isItTwo 2 = True
  isItTwo _ = False

  f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
  f x y = ((snd x, snd y), (fst x, fst y))
  f (a, b) (c, d) = ((b, d), (a, c))

  fst3 :: (a, b, c) -> a
  fst3 (x, _, _) = x

  returnLast :: a -> b -> c -> d -> d 
  returnLast _ _ _ d = d

  ```
- case expressions
  ```
  funcZ x =
    case x + 1 == 1 of
      True -> "AWESOME" 
      False -> "wut"
  ```

- HOF
  ```
  myf :: (a -> b) -> a -> c -> b
  myf f a c = f a
  ```

  Exercise: Artful dodgy - [sol](./ch7/artfulDodgy.hs)

- Guards
  ```
  myAbs :: Integer -> Integer 
  myAbs x
    | x < 0 = (-x) 
    | otherwise = x

  dogYrs :: Integer -> Integer 
  dogYrs x
    | x<=0 =0
    | x<=1 = x * 15
    | x<=2 = x * 12
    | x<=4 = x * 8
    | otherwise = x * 6
  ```

  Exercise: Guard Duty - [sol](./ch7/guardDuty.hs)

- Function composition
  ```
  f g x
  (f.g) x
  f (g x)
  f.g $ x

  negate sum xs
  negate . sum $ xs
  (negate . sum) xs

  take 5 . enumFrom $ 3

  take 5 . filter odd . enumFrom $ 3 

  ```

- Point-free style
  composing functions without parameter
  ``` 
  (f . g) x = f (g x)
  f . g = \x -> f (g x)
  f . g . h = \x -> f (g (h x))

  ```
  [arith2.hs](./ch7/arith2.hs)

## Chap 8 - Recursion
```
factorial :: Integer -> Integer 
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Integral a => a -> a 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)
```

## Chap 9 - List
WHNF(Weak Head Normal Form) - the expression is only evaluated as far as is necessary to reach a data constructor or lamda.
  top most thing outer most thing is top most constructor
  top most thing lamda

NF(Normal Form) - the expression is fully evaluated.
```
(1, 2) -- WHNF & NF
(1, 1 + 1) -- WHNF & not NF
\x -> x * 10 -- WHNF & NF
"Papu" ++ "chon" -- not WHNF & not NF
num :: [Int]; num = [1,2,3] -- WHNF & not NF (Note: `[1,2,3]` gets compiled to `1:(_{2:3:[]}_)`)
num :: [Int]; myNum = [1..10] – not WHNF & not NF  (Note: `[1..10]` gets compiled to `enumFromTo 1 10`)

[1,2,3,4,5] -- WHNF & NF (good!) 
1:2:3:4:_ -- WHNF & not NF (good!)
enumFromTo 1 10 -- not WHNF & not NF (good!)
length [1,2,3,4,5] -- not WHNF & NF (to be NF, it must first be WHNF.)
sum (enumFromTo 1 10) -- not WHNF & not NF (good!)
['a'..'m'] ++ ['n'..'z'] -- not WHNF & not NF (good!)
(_, 'b') -- WHNF & not NF (good! the root of the syntax tree is a data constructor)


f(x) = x^2 - 2x - 4

g(x) = 1 - x^3

h(x) = (3x + f(1 - 3x)) / g(x - 4)


suppose we need h(4)

traditionally, we start on the inner most pieces, the leaves of the syntax tree.
h(4) = (3*4 + f(1-3*4))/g(4-4)
      = (12 + f(1-12))/g(0)
      = (12 + f(-11))/(1 - 0^3)
      = (12 + ((-11)^2 - 2*(-11) - 4)) / (1 - 0)

but what if i told you that we don't have to start on the inside first? (morpheus look)
h(x) = (3x + f(1 - 3x)) / g(x - 4)
h(x) = (3x + ( (1-3x)^2 - 2*(1-3x) - 4) / (1 - (x - 4)^3)
                ^ we evaluated f without evaluating its argument first!
                          ^ same, evaluated g before evaluating its arg!



```

strict evaluation is when you evaluate your arguments before evaluating the function.
This is how C, Java, Javascript, Python almost every programming language works.

lazy evaluation is when you evaluate your functions before evaluating their arguments.
This is how (most things in) Haskell (usually) works.

Haskell is not necessarily lazy. Compiler chooses strictness according to the categories in Strictness analyzer.

## Chap 10 - Folding Lists
Fold is a HOF that given a function to accumulate the results and returns the built up value.

```
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

A catamorphism is a generalization of folds to arbitrary datatypes. Where a fold allows you to break down a list into an arbitrary
datatype, a catamorphism is a means of breaking down the structure of any datatype.

```
data Bool = False | True
bool :: a -> a -> Bool -> a
```

```
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
```

## Chap 11 - Algebraic Datatypes
type constructor vs data costructor 
  - type constructors -- compile-time
  - data constructors -- runtime

Type and data constructors that take no arguments are constants.

newtype - a type that can only ever have a single unary data constructor
  Benefits: 
    - Enables writing customize instances
    - Lightweight
    check [newtype.hs](ch11/newtype.hs)

You can think of Haskell code 
```data Aa = Bb Cc Dd | Ee Ff Gg Hh | Ii | Jj Kk``` as being compiled to code that's roughly equivalent to some Javascript that looks like this:
function Bb(Cc, Dd) {
    return ["Bb", Cc, Dd];
}
function Ee(Ff, Gg, Hh) {
    return ["Ee", Ff, Gg, Hh];
}
function Ii() {
    return ["Ii"];
}
function Jj(Kk) {
    return ["Jj", Kk];
}

notice that Aa doesn't appear anywhere in there.

when you write this pattern matching code in haskell:

```
doSomeStuff a = case a of
    Bb x y -> f1 x y
    Ee u v w -> f2 u v w
    Ii -> f3
    Jj z -> f4 z
```

it compiles to javascript that would look something like this

```
function doSomeStuff(a) {
    switch a[0] {
        case "Bb":
            var x = aa[1];
            var y = aa[2];
            return f1(x,y);
        case "Ee":
            var u = aa[1];
            var v = aa[2];
            var w = aa[3];
            return f2(u,v,w);
        case "Ii":
            return f3();
        case "Jj":
            let z = aa[1];
            return f4(z);
    }
}
```
So, you can think of every Haskell data value as natively being represented by an array where the first element is the constructor tag.

Record syntax
```data Person = Person {name::String, age::Int}```

types progression could implement builder pattern
```
data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False
```

```
There     :: Float -> Int -> Bool -> ThereYet
notYet    ::          Int -> Bool -> ThereYet
notQuite  ::                 Bool -> ThereYet
yusssss   ::                         ThereYet
```

In Haskell, we want the type checker to catch us doing things
wrong, so we can fix our mistakes before problems multiply and things
go wrong at runtime. But the type checker can best help those who
help themselves

## Chapter 12 - Signaling Adversity (Maybe & Either)

## Chapter 13 - Buidling Project (Stack & Cabal)

## Chapter 14 - Testing
Well-specified types can enable programmers to avoid many obvious and tedious tests that might otherwise be necessary to maintain in untyped programming languages.

- Unit Testing
  hspec
  hunit

  ```
  main :: IO ()
  main = hspec $ do
      describe "Addition" $ do
          it "1 + 1 is greater than 1" $ do
              (1 + 1) > 1 `shouldBe` True
  ```

- Property Testing
  Tests the formal properties of programs without requiring formal proofs by allowing you to express a truth-valued, universally quantified function that is checked against randomly generated inputs that's generated by standard functions inside the QuickCheck library.

  ```
  describe "QuickCheck" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  ```

  QuickCheck relies on a type class called Arbitrary and a newtype called Gen for generating its random data.

## Chap 15 - Monoid, Semigroup



A monoid is a binary associative operation with an identity.
  - monoid is a function that takes two arguments and follows two laws: associativity and identity.
  - Daniel's def:
  A monoid is a type a with a specially designated constant ```mempty :: a``` and a specially designated function 
    ``` (<>) :: a -> a -> a ``` that must satisfy 
    ``` 
    mempty <> x = x, x <> mempty = x, and 
    (x <> y) <> z = x <> (y <> z) for all x, y, z :: a 
    ```

The type class Monoid is defined like this:
```
class Semigroup m => Monoid m where 
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

Summary:
Q: If someone tells you "`Blam` is a `Monoid`," what are all the implications that has for `Blam`.

A:	
  0. `Blam` is a _type_.
	1. There is a function `(<>) :: Blam -> Blam -> Blam` where `(x <> y) <> z = x <> (y <> z)` for all `x, y, z :: Blam`.
  2. There is a constant `mempty` where `mempty <> x = x <> mempty = x` for all `x :: Blam`.


## Chap 16 - Functor
```
Q: If someone tells you "`Blorg` is a `Functor`," what are all the implications such a claim has for `Blorg`?

A: 
  0. `Blorg` is a type-level function with kind `* -> *`
	  	That is, you can plug a type into `Blorg`, and it returns a type.
  1. There is a function `fmap :: (a -> b) -> Blorg a -> Blorg b`
	2. `fmap` satisfies `fmap id` = `id`
	3. `fmap` satisfies `fmap (f . g) = fmap f . fmap g
```

```
ave = Just "Ave"
n = Nothing
w = Just "woohoo"

lms = [ave, n, w]  -- lms :: [Maybe String]

replaceWithP = const 'p' -- replaceWithP :: a -> Char

:t replaceWithP lms 
> Char                   -- a -> Char

:t fmap replaceWithP lms
> [Char]                 -- a -> Char ~=> [...] -> [Char]

:t (fmap . fmap) replaceWithP lms
> [Maybe Char]           -- a -> Char ~=> [ Maybe ... ] -> [Maybe Char]

:t (fmap . fmap . fmap) replaceWithP lms
> [Maybe [Char]]         -- a -> Char ~=> [ Maybe [Char] ] -> [Maybe [Char]]

https://www.reddit.com/r/HaskellBook/comments/ex3dxp/ch_16_a_bit_of_confusion/fs9kjxn/?context=3

-- Before:
     replaceWithP ::                Char ->   Char

-- After:
fmap replaceWithP :: Functor f => f Char -> f Char

If you keep composing fmap, e.g. fmap . fmap . fmap, you can penetrate three nested layers of functors – e.g. a (1) List of (2) Maybe (3) List of Char. And since [Char] is synonymous with String, saying "List of Maybe List of Char" is the same as "List of Maybe String".

fmap . fmap . fmap $ replaceWithP :: Functor f, Functor g, Functor h
                                  => f (g (h x))
                                  -> f (g (h Char))

-- if f = List, g = Maybe, h = List, and x = Char:
(fmap . fmap . fmap $ replaceWithP) [Just "hi"] == [Just "pp"]
```

```
(.) ::(b->c)->(a->b)->a->c
--     fmap    fmap
fmap :: Functor f => (m -> n) -> f m -> f n 
                ~=> (m -> n) -> (f m -> f n)
                        b             c
fmap :: Functor g => (x -> y) -> g x -> g y
                ~=> (x -> y) -> (g x -> g y)
                        a             b
(fmap . fmap) :: 
  (Functor f, Functor g) => (m -> n) -> (f m -> f n)
                         -> (x -> y) -> (g x -> g y)
                         -> (x -> y) -> (f m -> f n)

  With (b->c)->(a->b) pattern, for b, substitute m = g x, n = g y

  (Functor f, Functor g) => (g x -> g y) -> f (g x) -> f (g y)
                         -> (x -> y) -> g x -> g y
                         -> (x -> y) -> f g x -> f g y

  So the punchline of this is that the result of fmap . fmap is a function that takes a vanilla x -> y and produces an "upgraded" function f (g x) -> f (g y) for two functors f and g. 



```


