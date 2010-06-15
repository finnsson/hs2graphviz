module Main where

import Hs2Dot.Internals

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)



main :: IO ()
main = do
  args <- getArgs
  files <- filterM doesFileExist args
  code <- files2dot files
  putStr $ if null files then manual else code

manual :: String
manual =
  "usage: hs2dot [files.hs]*\n" ++
  "\n" ++
  "The files must be Haskell source code that haskell-src-exts can parse.\n" ++
  "Some restrictions apply to the source files that can be parsed!\n"
