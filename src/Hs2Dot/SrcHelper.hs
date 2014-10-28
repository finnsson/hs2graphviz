module Hs2Dot.SrcHelper where

import Hs2Dot.Dot
import Hs2Dot.Helper


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


getFullNameId :: (String, String) -> String
getFullNameId (modName, decName) =
  dot2Dash modName ++ "_" ++ decName


moduleNames :: E.Module -> [(String,String)]
moduleNames (E.Module _ (ModuleName moduleName) _ _ _ _ decls) = zip (repeat moduleName) $ mapMaybe declName decls

declName :: E.Decl -> Maybe String
declName (E.DataDecl _ _ _ name _ _ _ ) = Just $ pretty name
declName (E.TypeDecl _ name _ _ ) = Just $ pretty name
declName (E.ClassDecl _ _ name _ _ _) = Just $ pretty name
declName _ = Nothing

decl2fullname :: String -> E.Decl -> Maybe FullName
decl2fullname moduleName (E.DataDecl _ _ _ name tyVarBind qualConDecl derivings) =
  Just $ FullName name' (dot2Dash moduleName ++ "_" ++ name') moduleName moduleName False
  where name' = pretty name
decl2fullname moduleName (E.TypeDecl _ name _ _) = name2fullname moduleName name
decl2fullname moduleName (E.ClassDecl _ _ name _ _ _) = name2fullname moduleName name 
decl2fullname _ _ = Nothing

name2fullname :: String -> E.Name -> Maybe FullName
name2fullname moduleName name = Just $ FullName name' (dot2Dash moduleName ++ "_" ++ name') moduleName moduleName False
  where name' = pretty name



-- | Can remove and transform FullName depending on ImportDecl.
transformNameOnImport :: String -> ImportDecl -> FullName -> Maybe FullName       
transformNameOnImport nameOfThisModule (ImportDecl iLoc (ModuleName iName) iQual _ _ _ iAlias iSpecs) fullname = result
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
      if any (\i -> compare (pretty i) (fullNameDecl fullname)) specs  -- include?
      then Just $ resultOfQualified
      else Nothing
      where compare = if hide then (/=) else (==)
    -- filterImportedSpecs False specs = any (\i -> importSpecName i == fullNameDecl fullname) specs   -- filter away if not in list

    resultOfQualified :: FullName
    resultOfQualified =
      case iAlias of      
        Nothing -> fullname
        Just (ModuleName n) -> fullname { fullNamePrefix = n, fullNameQualified = True  }


-- bangType2String :: E.Type -> String
-- bangType2String = prettyPrint

-- | Returns the (name,type) pretty-printed 
nameBangType2String :: ([Name],E.Type) -> (String, String)
nameBangType2String (names,bangType) = (maybe "" prettyPrint $ listToMaybe names, pretty bangType)

bangType2StringIfInNames :: [FullName] -> E.Type ->  Maybe String
bangType2StringIfInNames names bangType = result
  where
    nameOfBangType = pretty bangType
    result = if isInNames names nameOfBangType
             then Just nameOfBangType
             else Nothing

-- | Pretty
--
-- Converts code into pretty-printed string

class Pretty a where
  pretty :: a -> String

instance Pretty E.Type where
  pretty = prettyPrint

instance Pretty E.Name where
  pretty (E.Symbol n) = n
  pretty (E.Ident n) = n

instance Pretty E.ModuleName where
  pretty (E.ModuleName n) = n


instance Pretty ImportSpec where
  pretty (IVar _ n) = prettyPrint n
  pretty (IAbs n) = prettyPrint n
  pretty (IThingAll n) = prettyPrint n
  pretty (IThingWith n _) = prettyPrint n
