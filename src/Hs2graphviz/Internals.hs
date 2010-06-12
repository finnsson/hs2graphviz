module Hs2Graphviz.Internals where

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import Control.Exception (throw, ErrorCall (..))

import Language.Haskell.Exts (parseFileContents)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax as E
import Language.Haskell.Syntax
-- import Text.Regex.Posix
import Maybe
import Language.Haskell.Exts.Extension
import Data.List
import Data.Maybe



files2dot :: [FilePath] -> IO String
files2dot files = do
  files <- mapM readFile files
  let modules = map file2module files
      namesInModules = modules >>= moduleNames -- map moduleNames modules
  -- let code = foldr (++) "" files
  return $ header ++ (modules >>= module2dot namesInModules)  ++ "}" -- foldr (++) [] $ map file2dot files

file2module :: String -> E.Module
file2module file = res
  where
    parsedModule = parseFileContents file -- fileCode
    res =
      case parsedModule of
        (ParseFailed _ msg) -> throw $ ErrorCall msg
        (ParseOk m) -> m


-- file2dot :: String -> String
-- file2dot file = res
--   where
--     parsedModule = parseFileContents file -- fileCode
--     res =
--       case parsedModule of
--         (ParseFailed _ msg) -> throw $ ErrorCall msg
--         (ParseOk m) -> module2dot m

module2dot :: [(String,String)] -> E.Module -> String
module2dot names (E.Module srcLoc moduleName opts mwarnings mExps imps decls) = result
  where
    result =
      (subgraphHeader moduleName) ++
      (decls >>= decl2dot names) ++
      (subgraphFooter)
    -- node per decl

    -- -> per record
 
-- decl2dot = show

subgraphHeader :: ModuleName -> String
subgraphHeader (ModuleName n) = "subgraph cluster_" ++ n' ++ " {\ncolor=lightgrey;\nlabel = \"" ++ n ++ "\";\n"
  where n' = dot2Dash n

dot2Dash :: String -> String
dot2Dash value = map (\v -> if v == '.' then '_' else v) value

subgraphFooter :: String
subgraphFooter = "}\n"

moduleNames :: E.Module -> [(String,String)]
moduleNames (E.Module _ (ModuleName moduleName) _ _ _ _ decls) = zip (repeat moduleName) $ catMaybes $ map declName decls

declName :: E.Decl -> Maybe String
declName (E.DataDecl _ _ _ name _ _ _ ) = Just $ name2string name
declName _ = Nothing

decl2dot :: [(String,String)] -> E.Decl -> String

decl2dot names (E.DataDecl _ _ _ name tyVarBind qualConDecl derivings) = result
  where
    result = name' ++ qualConDecl' -- showNode name [] [] 
    name' = showDataDecl dataTypeRefs (name2string name) [] []
    qualConDecl' = qualConDecl >>= qualConDecl2dot names
    dataTypeRefs :: [String]
    dataTypeRefs = map getDataTypeRefs qualConDecl
    getDataTypeRefs (E.QualConDecl _ _ _ c) = getConDeclName c
    getConDeclName (E.ConDecl name _) = name2string name
    getConDeclName (E.InfixConDecl _ name _ ) = name2string name
    getConDeclName (E.RecDecl name _ ) = name2string name
-- node for name
-- node per qualConDecl
-- -> (inheritance) per qualConDecl to name
decl2dot _ _ = "" 

qualConDecl2dot :: [(String,String)] -> E.QualConDecl -> String
-- qualConDecl2dot = show --(E.QualConDecl 
qualConDecl2dot names (E.QualConDecl _ _ _ c) = conDecl2dot names c

conDecl2dot :: [(String,String)] -> E.ConDecl -> String
conDecl2dot names (E.ConDecl name bangTypes) = showConDecl names (name2string name) [] $ map (\b -> ("",bangType2String b)) bangTypes
conDecl2dot names (E.InfixConDecl bangTypeL name bangTypeR) = showConDecl names (name2string name) [] [("",bangType2String bangTypeL),("", bangType2String bangTypeR) ]
conDecl2dot names (E.RecDecl name nameBangTypes) = showConDecl names (name2string name) [] $ map nameBangType2String nameBangTypes

bangType2String :: E.BangType -> String
bangType2String = prettyPrint


nameBangType2String :: ([Name],E.BangType) -> (String, String)
nameBangType2String (names,bangType) = (maybe "" prettyPrint $ listToMaybe names, prettyPrint bangType)

name2string :: E.Name -> String
name2string (E.Symbol n) = n
name2string (E.Ident n) = n

arrow2dot :: E.Decl -> String
arrow2dot = error "Not implemented yet."

showConDecl = showNode "record" "condecl" []

showDataDecl refs = showNode "record" "datadecl" refs []
 
showNode :: String -> String -> [String] -> [(String,String)] -> String -> [String] -> [(String,String)] -> String
showNode shape prefix refs names name instances records =
    "\"" ++ prefix ++ "_" ++ name ++ "\" [\n" ++
      "label = \"<f0> " ++ name ++ (foldl (++) "" $ map showLabel recs) ++ "\"" ++ "\n" ++
      "shape = \"" ++ shape ++ "\"" ++ "\n" ++
      "];\n" ++
      (recsForLine >>= showRecord') ++
      (refs >>= showRef')
    where
      recs = zip [1..] records
      recsForLine = zip [1..] $ filter (isRecordInNames names) records
      showRef' = showRef "condecl" name
      showRecord' = showRecord prefix name

isRecordInNames :: [(String,String)] -> (String,String) -> Bool
isRecordInNames names record = result
  where
    result = any (\n -> snd n == snd record) names

showRef :: String -> String -> String -> String
showRef prefix name dataRelation =
  "\""++ prefix ++"_" ++ dataRelation ++ "\":f"++ show 0 ++" -> \"datadecl_"++ name  ++"\":f0 ["++ arrowhead ++ "];\n"
  where
    arrowhead = "arrowhead=onormal"

showLabel :: (Integer, (String, String)) -> String
showLabel (i, (key,value)) =
  " | <f" ++ show i ++ "> " ++ key ++ " :: " ++ value
 
showRecord :: String -> String -> (Integer, (String,String)) -> String
showRecord prefix dataRelation (i, (key, value)) =
  "\""++ prefix ++"_" ++ dataRelation ++ "\":f"++ show i ++" -> \"datadecl_"++ value  ++"\":f0 [];\n"



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


