module Test.BarCode where

import qualified Test.TestCodez as X

-- DATA TYPES

data Foo = Feet

data Bar = Bar Foo | Fooz { left :: X.Foo }

data Goo = Goo Foo

data Hoo = Hoo Goo
