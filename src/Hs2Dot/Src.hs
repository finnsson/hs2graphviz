module Hs2Dot.Src where

import Hs2Dot.Dot
import Hs2Dot.DotHelper
import Hs2Dot.Helper
import Hs2Dot.SrcHelper

import System.Environment (getArgs)
import System.IO (FilePath)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import Control.Exception (throw, ErrorCall (..))

import Language.Haskell.Exts (parseFileContentsWithMode)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax as E
import Language.Haskell.Syntax
import Language.Haskell.Exts.Extension
import Data.List
import Data.Maybe
import Data.List.Split


data Config = ConfigHigh
             | ConfigNormal 
             | ConfigLow




files2dot :: Config -> [FilePath] -> IO String
files2dot conf files = do
  files <- mapM readFile files
  let modules = map file2module files
      namesInModules = modules >>= moduleNames -- map moduleNames modules
      names' = map (\n -> FullName (snd n) (getFullNameId n) (fst n) (fst n) False) namesInModules
  -- let code = foldr (++) "" files
  return $ header ++ (modules >>= module2dot conf names')  ++ "}" -- foldr (++) [] $ map file2dot files
  -- return $ error $ show modules -- 
  -- return $ error $ show names'

file2module :: String -> E.Module
file2module file = res
  where
    parsedModule = parseFileContentsWithMode (defaultParseMode { extensions = knownExtensions } ) file -- fileCode
    res =
      case parsedModule of
        (ParseFailed _ msg) -> throw $ ErrorCall msg
        (ParseOk m) -> m


module2dot :: Config -> [FullName] -> E.Module -> String
module2dot conf names (E.Module srcLoc moduleName opts mwarnings mExps imps decls) = result
  where
    result =
      (subgraphHeader moduleName) ++
      (decls >>= (\n -> decl2dot conf n (pretty moduleName) ) allNames) ++
      (subgraphFooter)
    
    -- filter away names in modules not imported into this module
    allNames = importedNames ++ (mapMaybe (decl2fullname (pretty moduleName)) decls)
    importedNames = catMaybes $ mapCross (transformNameOnImport $ pretty moduleName ) imps names 




decl2dot conf names moduleName (E.TypeDecl _ name tyVarBind t ) = result
  where
    result = name'
    name' = showTypeDecl (pretty name) moduleName [("",t')]  -- []
    t' = escapeName $ prettyPrint t

decl2dot conf names moduleName (E.ClassDecl _ _ name _ _ _) = name'
  where
    name' = showClassDecl (pretty name) moduleName []

decl2dot conf names moduleName (E.InstDecl _ _ qname types _) = show'
  where
    show' = if 1 == length types && isInNames names name' && isInNames names dataTypeName
            then showRef'
            else ""
    showRef' = showArrow "odiamond" "datadecl" moduleName  (prettyPrint $ head types)   "datadecl" name' (fullNameModule fullClassName)
    name' = prettyPrint qname -- change this!!!
    dataTypeName = prettyPrint $ head types
    fullClassName = fromJust $ getName names name'

decl2dot conf names moduleName (E.DataDecl _ _ context name tyVarBind qualConDecl derivings) = result
  where
    result = name' ++ qualConDecl' ++ asst' -- showNode name [] [] 
    name' = showDataDecl dataTypeRefs (pretty name) moduleName []
    asst' = case conf of
      ConfigHigh -> []
      _ -> context >>= (asst2dot names (prettyPrint name) moduleName)
    qualConDecl' = case conf of
      -- ConfigHigh -> qualConDecl >>= (\c -> qual
      _ -> qualConDecl >>= (\c -> qualConDecl2dot conf (pretty name) names c moduleName)
    dataTypeRefs :: [String]
    dataTypeRefs = case conf of
      ConfigHigh -> []
      _ -> map getDataTypeRefs qualConDecl
    getDataTypeRefs (E.QualConDecl _ _ _ c) = getConDeclName c
    getConDeclName (E.ConDecl name _) = pretty name
    getConDeclName (E.InfixConDecl _ name _ ) = pretty name
    getConDeclName (E.RecDecl name _ ) = pretty name
-- node for name
-- node per qualConDecl
-- -> (inheritance) per qualConDecl to name
decl2dot _ _ _  _ = "" 

qualConDecl2dot :: Config -> String -> [FullName] -> E.QualConDecl -> String -> String
-- qualConDecl2dot = show --(E.QualConDecl 
qualConDecl2dot ConfigHigh declName names (E.QualConDecl _ _ _ c) moduleName = conDeclRef2dot declName names c moduleName
qualConDecl2dot conf declName names (E.QualConDecl _ _ _ c) moduleName = conDecl2dot names c moduleName

conDeclRef2dot :: String -> [FullName] -> E.ConDecl -> String -> String
conDeclRef2dot declName names (E.ConDecl name bangTypes) moduleName =
  bangNames >>= showArrowFromDecl2Decl
  where
    bangNames :: [String]
    bangNames = catMaybes $ map (bangType2StringIfInNames names) bangTypes
    showArrowFromDecl2Decl :: String -> String
    showArrowFromDecl2Decl typ = showArrow "onormal" "datadecl" moduleName declName "datadecl" typ moduleName

  -- error "implement conDeclRef2dot"

conDeclRef2dot declName names (E.InfixConDecl bangTypeL name bangTypeR) moduleName =
  ""
conDeclRef2dot declName names (E.RecDecl name nameBangTypes) moduleName = 
  bangNames >>= showArrowFromDecl2Decl
  where
    bangNames = catMaybes $ map (bangType2StringIfInNames names ) (map snd nameBangTypes) 
    showArrowFromDecl2Decl :: String -> String
    showArrowFromDecl2Decl typ = showArrow "onormal" "datadecl" moduleName declName "datadecl" typ moduleName
      
    
-- ERROR change showArrow-call!!!

-- showArrow arrowHead fromPrefix fromModuleName fromDataRelation toPrefix toName toModuleName = 

conDecl2dot :: [FullName] -> E.ConDecl -> String -> String
conDecl2dot names (E.ConDecl name bangTypes) moduleName =
  showConDecl names (pretty name) moduleName $ map (\b -> ("",pretty b)) bangTypes
conDecl2dot names (E.InfixConDecl bangTypeL name bangTypeR) moduleName =
  showConDecl names (pretty name) moduleName [("",pretty bangTypeL),("", pretty bangTypeR) ]
conDecl2dot names (E.RecDecl name nameBangTypes) moduleName =
  showConDecl names (pretty name) moduleName $ map nameBangType2String nameBangTypes


-- ERROR: rewrite this code using Monad m => m String and get rid of isInNames with just getName !
asst2dot :: [FullName] -> String -> String -> Asst -> String
asst2dot fullnames declname moduleName (ClassA qname types) =
  if isInNames fullnames classname
  then varnames >>= (asst2dot' (fromJust $ getName fullnames classname))
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





arrow2dot :: E.Decl -> String
arrow2dot = error "Not implemented yet."




subgraphHeader :: ModuleName -> String
subgraphHeader (ModuleName n) = "subgraph cluster_" ++ n' ++ " {\ncolor=lightgrey;\nlabel = \"" ++ n ++ "\";\n"
  where n' = dot2Dash n 
