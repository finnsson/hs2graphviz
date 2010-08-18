module Hs2Dot.DotHelper where



import Hs2Dot.Dot
import Hs2Dot.Helper

import Data.List
import Data.Maybe
import Data.List.Split

showConDecl :: [FullName] -> String -> String -> [(String,String)] -> String
showConDecl = showNode "record" "condecl" []

showDataDecl refs = showNode "record" "datadecl" refs []
showClassDecl = showNode "record" "datadecl" [] [] 
showTypeDecl = showNode "record" "datadecl" [] []


showNode :: String -> String -> [String] -> [FullName] -> String -> String -> [(String,String)] -> String
showNode shape prefix refs names name moduleName records =
    "\"" ++ prefix ++ "_" ++ (dot2Dash moduleName) ++ "_" ++ name ++ "\" [\n" ++
      "label = \"<f0> " ++ name ++ (foldl (++) "" $ map showLabel recs) ++ "\"" ++ "\n" ++
      "shape = \"" ++ shape ++ "\"" ++ "\n" ++
      "];\n" ++
      (recsForLine >>= showRecord') ++
      (refs >>= showRef')
    where
      recs = zip [1..] records
      recsForLine = zip [1..] $ filter (isInNames names) records
      showRef' = showRef "condecl" moduleName name
      showRecord' r = showRecord prefix name moduleName (getModuleNameForRecord r names ) r
