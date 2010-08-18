module Main where

import Hs2Dot.Src

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)



main :: IO ()
main = do
  args <- getArgs
  files <- filterM doesFileExist args
  code <- files2dot (conf args) files
  putStr $ if null files then manual else code
  where
    conf args = if any (=="--high") args
           then ConfigHigh
           else if any (=="--low") args
                then ConfigLow
                else ConfigNormal

manual :: String
manual =
  "usage: hs2dot [files.hs]*\n" ++
  "\n" ++
  "The files must be Haskell source code that haskell-src-exts can parse.\n" ++
  "Some restrictions apply to the source files that can be parsed!\n"


-- Architecture:
-- Main
--   All IO
-- Hs2Dot.Src
--   Primary convertion-flow.
--   Every 1-1-relation between functions and boxes and lines in the dot-file.
--  Hs2Dot.SrcHelper
--    Helper-functions for Language.Haskell.Exts-datatypes
--  Hs2Dot.Dot
--    Functions to create dot-code.
--  Hs2Dot.DotHelper
--    Helper-functions to create dot-code. Makes use of Hs2Dot.Dot
--  Hs2Dot.Helper
--    Generic helper-methods not relying on any specific API (Language.Haskell.Exts nor Dot)
