module Hs2Dot.Dot where


import Hs2Dot.Helper

import Data.List
import Data.Maybe
import Data.List.Split



    

showRef :: String -> String -> String -> String -> String
showRef prefix moduleName name dataRelation =
  "\""++ prefix ++"_"  ++ (dot2Dash moduleName) ++ "_" ++ dataRelation ++ "\":f"++ show 0 ++" -> \"datadecl_"++ (dot2Dash moduleName) ++ "_" ++ name  ++"\":f0 ["++ arrowhead ++ "];\n"
  where
    arrowhead = "arrowhead=onormal"

-- Arrows

showArrow :: String -> String -> String -> String -> String -> String -> String -> String
showArrow arrowHead fromPrefix fromModuleName fromDataRelation toPrefix toName toModuleName =
  "\""++ 
  fromPrefix ++
  "_"  ++ 
  (dot2Dash fromModuleName) ++ 
  "_" ++ 
  fromDataRelation ++ 
  "\":f"++ 
  show 0 ++
  " -> \""++ 
  toPrefix ++
  "_"++ 
  (dot2Dash toModuleName) ++ 
  "_" ++ 
  toName  ++
  "\":f0 ["++ 
  arrowhead ++ 
  "];\n"
  where
    arrowhead = "arrowhead=" ++ arrowHead



-- Labels

showLabel :: (Integer, (String, String)) -> String
showLabel (i, (key,value)) =
  " | <f" ++ show i ++ "> " ++ key ++ " :: " ++ dataPart
  where
    splitedValue = splitOn "." value
    dataPart :: String
    dataPart = last $ splitedValue
 
showRecord :: String -> String -> String -> String -> (Integer, (String,String)) -> String
showRecord prefix dataRelation moduleName valueModuleName (i, (key, value)) =
  "\""++ prefix ++"_" ++ (dot2Dash moduleName) ++ "_" ++ dataRelation ++ "\":f"++ show i ++" -> \"datadecl_"++ valueModuleName  ++"\":f0 [];\n"



header =
  "digraph g {\n" ++
  "graph [\n" ++
  "rankdir = \"LR\"\n" ++
  "];\n" ++
  "node [\n" ++
  "fontsize = \"16\"\n" ++
  "shape = \"ellipse\"\n" ++
  "];\n" ++
  "edge [\n" ++
  "];\n"

subgraphFooter :: String
subgraphFooter = "}\n"




