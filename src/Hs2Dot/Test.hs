{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Hs2Graphviz.Test where

import Hs2Graphviz.Internals

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)

-- for test
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework (defaultMain)

import Language.Haskell.TH 


main = defaultMain [tests]

tests = $testGroupGenerator

case_serialize_datarelation =
  do let expected = "\"node_Test\" [\nlabel = \"<f0> Test\"\nshape = \"record\"\n];"
         actual = toGV $ DataRelation "Test" [] []
     expected @=? actual

case_serialize_with_records =
  do let expected = "\"node_Test\" [\nlabel = \"<f0> Test | <f1> name\"\nshape = \"record\"\n];\"node_Test\":f1 -> \"node_Test\":f0 [];\n"
         actual = toGV $ DataRelation "Test" [] [RelationRecord "name" "Test" ]
     expected @=? actual

case_serialize_TestCode =
  do let expected = "some code"
     actual <- files2dot ["Hs2graphviz/TestCode.hs"]
     expected @=? actual

-- "node0" [
-- label = "<f0> 0x10ba8| <f1>"
-- shape = "record"
-- ];



-- 1. get valid files
-- 2. extract data, type, newtype, class, instance
-- 3. create structure of relations
-- 4. print graphviz-code from structure
