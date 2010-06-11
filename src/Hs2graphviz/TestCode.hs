module Hs2Hraphviz.TestCode where

import Data.ByteString.Lazy

-- DATA TYPES

type Age = Integer

type Name = String

data File = File ByteString Name
  deriving (Show, Eq)

data FileWithRecords = FileWithRecordsConstructor { name :: File , age :: Integer}

class To42 a where
  to :: a -> Integer

instance To42 File where
  to = 42
