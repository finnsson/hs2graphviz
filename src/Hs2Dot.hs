module Hs2Dot where

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

