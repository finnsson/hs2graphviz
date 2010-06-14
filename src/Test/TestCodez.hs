module Test.TestCodez where

import Test.TestCode

-- DATA TYPES

data Foo = Foo | Bar { file :: File }

instance To42 Foo where
  to = 42
