{-# OPTIONS_GHC -fglasgow-exts -XTypeSynonymInstances #-}
module Hs2Dot.Helper where


import Data.List
import Data.Maybe
import Data.List.Split



-- | Functions related to FullName

data FullName =
  FullName {
    fullNameDecl :: String, -- name used in analyzed module
    fullNameId :: String, -- unique id in dot-file, original-module-name + original decl name
    fullNameModule :: String, -- original name of module
    fullNamePrefix :: String, -- prefix of module in analyzed module
    fullNameQualified :: Bool -- if is qualified import
  }
  deriving (Show, Eq)


class InNames a where
  isTheName :: FullName -> a -> Bool

  isInNames :: [FullName] -> a -> Bool
  isInNames names x = any (\n -> isTheName n x) names

  getName :: [FullName] -> a -> Maybe FullName
  getName names x = find (\n -> isTheName n x) names

instance InNames String where
  isTheName fullname left =
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

instance InNames (String,String) where
  isTheName name record =
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



getModuleNameForRecord :: (Integer, (String,String)) -> [FullName] -> String
getModuleNameForRecord (i, (key,value)) fullNames = result
  where
    result = fullNameId fullname
    fullname = fromJust (getName fullNames (key,value)) -- $ find (isInNames (key,value)) fullNames



-- | Other functions

-- | Replaces every '.' with a '_'.
--
-- > dot2Dash "Foo.X" == "Foo_X"
dot2Dash :: String -> String
dot2Dash value = map (\v -> if v == '.' then '_' else v) value

-- | Produces a list of every combination fn(x,y)
mapCross :: (a -> b -> c) -> [a] -> [b] -> [c]
mapCross fn xs ys =
  concatMap (\x -> (map (fn x) ys)) xs


escapeName :: String -> String
escapeName value = value >>= (\v -> if v == '>' then ['\\',v] else [v])
