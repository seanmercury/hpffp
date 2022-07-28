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
