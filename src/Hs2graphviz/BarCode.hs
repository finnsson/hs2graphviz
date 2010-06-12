module Hs2Graphviz.BarCode where

import qualified Hs2Graphviz.TestCodez as X

-- DATA TYPES

data Foo = Feet

data Bar = Bar Foo | Fooz { left :: X.Foo }
