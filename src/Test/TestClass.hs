module Test.TestClass where

class Foo a where
  to :: a -> Integer

data Foo a => Bar a = Baz a
