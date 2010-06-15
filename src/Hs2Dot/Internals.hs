module Hs2Dot.Internals where

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
import Language.Haskell.Exts.Extension
import Data.List
import Data.Maybe
import Data.List.Split


files2dot :: [FilePath] -> IO String
files2dot files = do
  files <- mapM readFile files
  let modules = map file2module files
      namesInModules = modules >>= moduleNames -- map moduleNames modules
      names' = map (\n -> FullName (snd n) (getFullNameId n) (fst n) (fst n) False) namesInModules
  -- let code = foldr (++) "" files
  return $ header ++ (modules >>= module2dot names')  ++ "}" -- foldr (++) [] $ map file2dot files
  -- return $ error $ show modules -- 
  -- return $ error $ show names'

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

module2dot :: [FullName] -> E.Module -> String
module2dot names (E.Module srcLoc moduleName opts mwarnings mExps imps decls) = result
  where
    result =
      (subgraphHeader moduleName) ++
      (decls >>= (\n -> decl2dot n (getModuleName moduleName) ) allNames) ++
      (subgraphFooter)
    
    -- filter away names in modules not imported into this module
    allNames = importedNames ++ (mapMaybe (decl2fullname (getModuleName moduleName)) decls)
    importedNames = catMaybes $ mapCross (transformNameOnImport $ getModuleName moduleName ) imps names 


decl2fullname :: String -> E.Decl -> Maybe FullName
decl2fullname moduleName (E.DataDecl _ _ _ name tyVarBind qualConDecl derivings) =
  Just $ FullName name' (dot2Dash moduleName ++ "_" ++ name') moduleName moduleName False
  where name' = name2string name
decl2fullname moduleName (E.TypeDecl _ name _ _) = name2fullname moduleName name
decl2fullname moduleName (E.ClassDecl _ _ name _ _ _) = name2fullname moduleName name 
decl2fullname _ _ = Nothing

name2fullname :: String -> E.Name -> Maybe FullName
name2fullname moduleName name = Just $ FullName name' (dot2Dash moduleName ++ "_" ++ name') moduleName moduleName False
  where name' = name2string name

mapCross :: (a -> b -> c) -> [a] -> [b] -> [c]
mapCross fn xs ys =
  concatMap (\x -> (map (fn x) ys)) xs

--  foldr (++) [] $ map (map fn as) bs

-- | Can remove and transform FullName depending on ImportDecl.
transformNameOnImport :: String -> ImportDecl -> FullName -> Maybe FullName       
transformNameOnImport nameOfThisModule (ImportDecl iLoc (ModuleName iName) iQual _ _ iAlias iSpecs) fullname = result
  where
    result = if fullNameModule fullname == nameOfThisModule
             then Just fullname
             else
               if iName /= fullNameModule fullname
               then Nothing
               else resultOfSpecs
    resultOfSpecs =
      case iSpecs of
        Nothing -> Just $ resultOfQualified
        Just specs -> filterImportedSpecs specs
    filterImportedSpecs :: (Bool, [ImportSpec]) -> Maybe FullName
    filterImportedSpecs (hide, specs) =
      if any (\i -> compare (importSpecName i) (fullNameDecl fullname)) specs  -- include?
      then Just $ resultOfQualified
      else Nothing
      where compare = if hide then (/=) else (==)
    -- filterImportedSpecs False specs = any (\i -> importSpecName i == fullNameDecl fullname) specs   -- filter away if not in list

    resultOfQualified :: FullName
    resultOfQualified =
      case iAlias of      
        Nothing -> fullname
        Just (ModuleName n) -> fullname { fullNamePrefix = n, fullNameQualified = True  }

importSpecName :: ImportSpec -> String
importSpecName (IVar n) = prettyPrint n
importSpecName (IAbs n) = prettyPrint n
importSpecName (IThingAll n) = prettyPrint n
importSpecName (IThingWith n _) = prettyPrint n



getModuleName :: E.ModuleName -> String
getModuleName (E.ModuleName n) = n

getFullNameId :: (String, String) -> String
getFullNameId (modName, decName) =
  dot2Dash modName ++ "_" ++ decName

data FullName =
  FullName {
    fullNameDecl :: String, -- name used in analyzed module
    fullNameId :: String, -- unique id in dot-file, original-module-name + original decl name
    fullNameModule :: String, -- original name of module
    fullNamePrefix :: String, -- prefix of module in analyzed module
    fullNameQualified :: Bool -- if is qualified import
  }
  deriving (Show, Eq)

subgraphHeader :: ModuleName -> String
subgraphHeader (ModuleName n) = "subgraph cluster_" ++ n' ++ " {\ncolor=lightgrey;\nlabel = \"" ++ n ++ "\";\n"
  where n' = dot2Dash n

dot2Dash :: String -> String
dot2Dash value = map (\v -> if v == '.' then '_' else v) value

subgraphFooter :: String
subgraphFooter = "}\n"

moduleNames :: E.Module -> [(String,String)]
moduleNames (E.Module _ (ModuleName moduleName) _ _ _ _ decls) = zip (repeat moduleName) $ mapMaybe declName decls

declName :: E.Decl -> Maybe String
declName (E.DataDecl _ _ _ name _ _ _ ) = Just $ name2string name
declName (E.TypeDecl _ name _ _ ) = Just $ name2string name
declName (E.ClassDecl _ _ name _ _ _) = Just $ name2string name
declName _ = Nothing

decl2dot :: [FullName] -> String -> E.Decl -> String

decl2dot names moduleName (E.TypeDecl _ name tyVarBind t ) = result
  where
    result = name'
    name' = showTypeDecl (name2string name) moduleName [] [("",prettyPrint t)]  -- []

decl2dot names moduleName (E.ClassDecl _ _ name _ _ _) = name'
  where
    name' = showClassDecl (name2string name) moduleName [] []

decl2dot names moduleName (E.InstDecl _ _ qname types _) = show'
  where
    show' = if 1 == length types && isNameInNames name' names
            then showRef'
            else ""
    showRef' = showArrow "odiamond" "datadecl" moduleName  (prettyPrint $ head types)   "datadecl" name' (fullNameModule fullClassName) -- "Hs2Graphviz_TestCode"
    name' = prettyPrint qname -- change this!!!
    fullClassName = getFullNameForName name' names

-- showArrow :: String -> String -> String -> String -> String -> String -> String -> String
-- showArrow arrowHead fromPrefix fromModuleName fromDataRelation toPrefix toName toModuleName =
 
--  "Hs2Graphviz_TestCode"
-- showRef :: String -> String -> String -> String -> String
-- showRef prefix moduleName name dataRelation =


decl2dot names moduleName (E.DataDecl _ _ context name tyVarBind qualConDecl derivings) = result
  where
    result = name' ++ qualConDecl' ++ asst' -- showNode name [] [] 
    name' = showDataDecl dataTypeRefs (name2string name) moduleName [] []
    asst' = context >>= (asst2dot names (prettyPrint name) moduleName)
    qualConDecl' = qualConDecl >>= (\c -> qualConDecl2dot names c moduleName) 
    dataTypeRefs :: [String]
    dataTypeRefs = map getDataTypeRefs qualConDecl
    getDataTypeRefs (E.QualConDecl _ _ _ c) = getConDeclName c
    getConDeclName (E.ConDecl name _) = name2string name
    getConDeclName (E.InfixConDecl _ name _ ) = name2string name
    getConDeclName (E.RecDecl name _ ) = name2string name
-- node for name
-- node per qualConDecl
-- -> (inheritance) per qualConDecl to name
decl2dot _ _  _ = "" 

qualConDecl2dot :: [FullName] -> E.QualConDecl -> String -> String
-- qualConDecl2dot = show --(E.QualConDecl 
qualConDecl2dot names (E.QualConDecl _ _ _ c) moduleName = conDecl2dot names c moduleName

conDecl2dot :: [FullName] -> E.ConDecl -> String -> String
conDecl2dot names (E.ConDecl name bangTypes) moduleName =
  showConDecl names (name2string name) moduleName [] $ map (\b -> ("",bangType2String b)) bangTypes
conDecl2dot names (E.InfixConDecl bangTypeL name bangTypeR) moduleName =
  showConDecl names (name2string name) moduleName [] [("",bangType2String bangTypeL),("", bangType2String bangTypeR) ]
conDecl2dot names (E.RecDecl name nameBangTypes) moduleName =
  showConDecl names (name2string name) moduleName [] $ map nameBangType2String nameBangTypes

asst2dot :: [FullName] -> String -> String -> Asst -> String
asst2dot fullnames declname moduleName (ClassA qname types) =
  if isNameInNames classname fullnames
  then varnames >>= (asst2dot' (getFullNameForName classname fullnames))
  else error $classname ++ show ( length fullnames) -- ""
  where
    classname = prettyPrint qname
    varnames = map prettyPrint types
    asst2dot' :: FullName -> String -> String
    asst2dot' fullname varname =
      showArrow "diamond" "datadecl" moduleName declname "datadecl" (classname) (fullNameModule fullname)

-- showArrow :: String -> String -> String -> String -> String -> String -> String -> String
-- showArrow arrowHead fromPrefix fromModuleName fromDataRelation toPrefix toName toModuleName =

asst2dot _ _ _ _ = ""

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
showClassDecl = showNode "record" "datadecl" [] [] 
showTypeDecl = showNode "record" "datadecl" [] []

showNode :: String -> String -> [String] -> [FullName] -> String -> String -> [String] -> [(String,String)] -> String
showNode shape prefix refs names name moduleName instances records =
    "\"" ++ prefix ++ "_" ++ (dot2Dash moduleName) ++ "_" ++ name ++ "\" [\n" ++
      "label = \"<f0> " ++ name ++ (foldl (++) "" $ map showLabel recs) ++ "\"" ++ "\n" ++
      "shape = \"" ++ shape ++ "\"" ++ "\n" ++
      "];\n" ++
      (recsForLine >>= showRecord') ++
      (refs >>= showRef')
    where
      recs = zip [1..] records
      recsForLine = zip [1..] $ filter (isRecordInNames names) records
      showRef' = showRef "condecl" moduleName name
      showRecord' r = showRecord prefix name moduleName (getModuleNameForRecord r names ) r

isRecordInNames :: [FullName] -> (String,String) -> Bool
isRecordInNames names record = result
  where
    result = any (isRecordInName record) names -- (\n -> fullNameDecl n == snd record) names

isNameInNames :: String -> [FullName] -> Bool
isNameInNames name  = any (isNameTheName name)

isNameTheName :: String -> FullName -> Bool
isNameTheName left fullname =
      if fullNameQualified fullname || modulePart /= ""
      then sameName && fullNamePrefix fullname == modulePart
      else sameName
      where
        sameName = fullNameDecl fullname == dataPart
        splitedName = splitOn "." left
        modulePart = if 1 < length splitedName
                     then head $ init splitedName
                     else ""
        dataPart = if 1 < length splitedName
                   then last splitedName
                   else head $ splitedName

isRecordInName record name =
  if fullNameQualified name || modulePart /= ""
  then sameName && fullNamePrefix name == modulePart -- fst record
  else sameName
  where
    sameName = fullNameDecl name == dataPart -- splitedName -- snd record
    splitedName = splitOn "." (snd record)
    modulePart :: String
    modulePart = if 1 < length splitedName
                 then head $ init splitedName
                 else ""
    dataPart :: String
    dataPart = if 1 < length splitedName
               then last splitedName
               else head $ splitedName

getFullNameForName :: String -> [FullName] -> FullName 
getFullNameForName name fullnames = fullname
  where
    fullname = fromJust $ find (isNameTheName name) fullnames

getModuleNameForRecord :: (Integer, (String,String)) -> [FullName] -> String
getModuleNameForRecord (i, (key,value)) fullNames = result
  where
    result = fullNameId fullname
    fullname = fromJust $ find (isRecordInName (key,value)) fullNames
    

showRef :: String -> String -> String -> String -> String
showRef prefix moduleName name dataRelation =
  "\""++ prefix ++"_"  ++ (dot2Dash moduleName) ++ "_" ++ dataRelation ++ "\":f"++ show 0 ++" -> \"datadecl_"++ (dot2Dash moduleName) ++ "_" ++ name  ++"\":f0 ["++ arrowhead ++ "];\n"
  where
    arrowhead = "arrowhead=onormal"


showArrow :: String -> String -> String -> String -> String -> String -> String -> String
showArrow arrowHead fromPrefix fromModuleName fromDataRelation toPrefix toName toModuleName =
  "\""++ fromPrefix ++"_"  ++ (dot2Dash fromModuleName) ++ "_" ++ fromDataRelation ++ "\":f"++ show 0 ++" -> \""++ toPrefix ++"_"++ (dot2Dash toModuleName) ++ "_" ++ toName  ++"\":f0 ["++ arrowhead ++ "];\n"
  where
    arrowhead = "arrowhead=" ++ arrowHead


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

