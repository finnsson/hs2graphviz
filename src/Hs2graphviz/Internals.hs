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

-- createGraphvizCode :: TypeRelations -> String
-- createGraphvizCode typeRelations =

files2dot :: [FilePath] -> IO String
files2dot files = do
  files <- mapM readFile files
  return $ header ++ (files >>= file2dot)  ++ "}" -- foldr (++) [] $ map file2dot files

file2dot :: String -> String
file2dot file = res -- [] -- change this!!!
  where
    parsedModule = parseFileContents file -- fileCode
    res =
      case parsedModule of
        (ParseFailed _ msg) -> throw $ ErrorCall msg
        (ParseOk m) -> module2dot m

module2dot :: E.Module -> String
module2dot (E.Module srcLoc moduleName opts mwarnings mExps imps decls) = result
  where
    result = decls >>= decl2dot
    -- node per decl

    -- -> per record
 
decl2dot :: E.Decl -> String
-- decl2dot = show

decl2dot (E.DataDecl _ _ _ name tyVarBind qualConDecl derivings) = result
  where
    result = name' ++ qualConDecl' -- showNode name [] [] 
    name' = showDataDecl "datadecl" (name2string name) [] []
    qualConDecl' = qualConDecl >>= qualConDecl2dot
-- node for name
-- node per qualConDecl
-- -> (inheritance) per qualConDecl to name
decl2dot _ = "" 

qualConDecl2dot :: E.QualConDecl -> String
-- qualConDecl2dot = show --(E.QualConDecl 
qualConDecl2dot (E.QualConDecl _ _ _ c) = conDecl2dot c

conDecl2dot :: E.ConDecl -> String
conDecl2dot (E.ConDecl name bangTypes) = showConDecl "condecl" (name2string name) [] $ map (\b -> ("",bangType2String b)) bangTypes
conDecl2dot (E.InfixConDecl bangTypeL name bangTypeR) = showConDecl "condecl" (name2string name) [] [("",bangType2String bangTypeL),("", bangType2String bangTypeR) ]
conDecl2dot (E.RecDecl name nameBangTypes) = showConDecl "condecl" (name2string name) [] $ map nameBangType2String nameBangTypes

bangType2String :: E.BangType -> String
bangType2String = prettyPrint
-- bangType2String (BangedTy t) = prettyPrint t
-- bangType2String (UnBangedTy t) = prettyPrint t
-- bangType2String (UnpackedTy t) = prettyPrint t

nameBangType2String :: ([Name],E.BangType) -> (String, String)
nameBangType2String (names,bangType) = (maybe "" prettyPrint $ listToMaybe names, prettyPrint bangType)

name2string :: E.Name -> String
name2string (E.Symbol n) = n
name2string (E.Ident n) = n

arrow2dot :: E.Decl -> String
arrow2dot = error "Not implemented yet."

showConDecl = showNode "record"

showDataDecl = showNode "record"
 
showNode :: String -> String -> String -> [String] -> [(String,String)] -> String
showNode shape prefix name instances records =
    "\"" ++ prefix ++ "_" ++ name ++ "\" [\n" ++
      "label = \"<f0> " ++ name ++ (foldl (++) "" $ map showLabel recs) ++ "\"" ++ "\n" ++
      "shape = \"" ++ shape ++ "\"" ++ "\n" ++
      "];" ++
      (foldl (++) "" $ map showRecord' recs)
    where
      recs = zip [1..] records
      showRecord' = showRecord prefix name

showLabel :: (Integer, (String, String)) -> String
showLabel (i, (key,value)) =
  " | <f" ++ show i ++ "> " ++ key ++ " :: " ++ value
 
showRecord :: String -> String -> (Integer, (String,String)) -> String
showRecord prefix dataRelation (i, (key, value)) =
  "\""++ prefix ++"_" ++ dataRelation ++ "\":f"++ show i ++" -> \"datadecl_"++ value  ++"\":f0 [];\n"


-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]	


--------------------------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
---------- OLD CODE ------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

extractStructureFromFiles :: [FilePath] -> IO StructRelations
extractStructureFromFiles files = do
  files <- mapM readFile files
  -- let pMod = parseModuleWithMode ( ParseMode "test" [TemplateHaskell] False False [] ) moduleCode
  return $ StructRelations $ foldr (++) [] $ map extractFromFile files

extractFromFile :: String -> [DataRelation]
extractFromFile fileCode = []
  where
    pMod = parseModuleWithMode ( ParseMode "test" [TemplateHaskell] False False [] ) fileCode 
    res =
      case pMod of
        (ParseFailed _ _) -> throw $ ErrorCall "Error"
        (ParseOk m) -> extractFromHsModule m

extractFromHsModule :: E.Module -> [DataRelation]
extractFromHsModule (E.Module _ _ _ _ _ _ decls) =
  map fromJust $ filter isJust $ map extractFromDecl decls

extractFromDecl :: E.Decl -> Maybe DataRelation
extractFromDecl (E.DataDecl _ _ _ name tyVarBind qualConDecl derivings) =
  Just $ DataRelation name' [] [] -- last is records
  where
    name' = case name of
              (Ident n) -> n
              (Symbol n) -> n
extractFromDecl _ = Nothing

class ToGraphviz a where
  toGV :: a -> String

instance ToGraphviz DataRelation where
  toGV (DataRelation name instances records) =
    "\"node_" ++ name ++ "\" [\n" ++
      "label = \"<f0> " ++ name ++ (foldl (++) "" $ map generateLabel recs) ++ "\"" ++ "\n" ++
      "shape = \"record\"" ++ "\n" ++
      "];" ++
      (foldl (++) "" $ map serializeRecords' recs)
    where
      recs = zip [1..] records
      serializeRecords' = serializeRecords name

-- instance ToGraphviz RelationRecord where
--   toGV (RelationRecord key value) =
   
generateLabel :: (Integer, RelationRecord) -> String
generateLabel (i, RelationRecord key value) =
  " | <f" ++ show i ++ "> " ++ key
 
serializeRecords :: String -> (Integer, RelationRecord) -> String
serializeRecords dataRelation (i,RelationRecord key value) =
  "\"node_" ++ dataRelation ++ "\":f"++ show i ++" -> \"node_"++ value  ++"\":f0 [];\n"

-- "node_bar":f1 -> "node4":f0 [
-- ];


-- "node0" [
-- label = "<f0> 0x10ba8| <f1>"
-- shape = "record"
-- ];

instance ToGraphviz StructRelations where
  toGV (StructRelations drs) =
    header ++ (foldr (++) "" (map toGV drs)) ++ "}"
    

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


data StructRelations =
  StructRelations {
    structRelationsData :: [DataRelation]
  }
  deriving (Show, Eq)

data DataRelation =
  DataRelation {
    dataRelationName :: String,
    dataRelationInstances :: [String],
    dataRelationRecords :: [RelationRecord]
  }
  deriving (Show, Eq)

data RelationRecord = RelationRecord String String
  deriving (Show, Eq)

-- 1. get valid files
-- 2. extract data, type, newtype, class, instance
-- 3. create structure of relations
-- 4. print graphviz-code from structure
