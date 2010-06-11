module Main where

import Hs2Graphviz.Internals

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)



main :: IO ()
main = do
  args <- getArgs
  files <- filterM doesFileExist args
  code <- files2dot files
  putStr code
  -- printGraphvizCodeFromStructure fixture
  -- putStrLn $ foldr (++) "" files 




printGraphvizCodeFromStructure :: StructRelations -> IO ()
printGraphvizCodeFromStructure s = putStrLn $ toGV s


fixture = StructRelations [DataRelation "Foo" [] [RelationRecord "name" "Foo" ] ]


-- 1. get valid files
-- 2. extract data, type, newtype, class, instance
-- 3. create structure of relations
-- 4. print graphviz-code from structure
