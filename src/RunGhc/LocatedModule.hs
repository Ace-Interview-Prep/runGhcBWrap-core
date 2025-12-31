{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.LocatedModule where

import RunGhc.SystemModule
import RunGhc.Locate
import System.FilePath
import Data.Aeson
import GHC.Generics
import qualified Data.List.Split as List
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T

localImport :: LocatedModule -> Import
localImport mod = Import Nothing (getPathSegments mod)

localQualifiedImport :: T.Text -> LocatedModule -> Import
localQualifiedImport qname mod = Import (Just qname) (getPathSegments mod)

-- This is an interesting case
-- because the result can absolutely be wrong
-- But we want to be careful where we allow that failure
-- sometimes we even want that failure!
--
-- One case we never want this failure is with a TestModule we make.
-- User modules however can be designed to fail this way if user
-- has been allowed to make a mistake and has
class Locatable a where
  locate :: [PathSegment] -> a -> LocatedModule
-- | These instances describe the
--   boring logistical differences between
-- 
--  1) A full script given
--  2) Only a function given
--  3) A module we create fully
--
--  While broad, this is the last script of including 
--  some Haskell symbols in our end executable
--
-- The utility of this is that often, linking/locating is the last step we
-- want to think of

instance Locatable Script where
  locate pathSeg script =
    FromLocatedScript $ LocatedScript pathSeg script
instance Locatable SystemModule where
  locate pathSeg sysMod =
    FromSystemModule pathSeg sysMod
instance Locatable Expressions where
  locate pathSeg expressions =
    FromSystemModule pathSeg $ ExpressionsOnly expressions

instance SemigroupModule LocatedModule where
  addImports imports = \case
    loc@(FromLocatedScript _) -> loc
    sys@(FromSystemModule _ _) -> addImports imports sys
  addExtensions extensions = \case
    loc@(FromLocatedScript _) -> loc
    sys@(FromSystemModule _ _) -> addExtensions extensions sys
  addFuncsDataDecls script = \case
    loc@(FromLocatedScript _) -> loc
    sys@(FromSystemModule _ _) -> addFuncsDataDecls script sys
  

getPathSegments :: LocatedModule -> [PathSegment]
getPathSegments = \case
  FromLocatedScript (LocatedScript pathSegs _) -> pathSegs
  FromSystemModule pathSegs _ -> pathSegs

-- Is there value in this construct?
data DependentLocatedModule = DependentLocatedModule
  { _locatedModule :: LocatedModule
  , _deps :: [[PathSegment]]
  }

data LocatedModule
  = FromLocatedScript LocatedScript
  -- ^ Nothing to process to create+write Module
  --   but we probably still need to make it as a lib and link it
  --   with expected targets if it is a user Module
  | FromSystemModule [PathSegment] SystemModule
  deriving Generic
  -- ^ Builder descriptions to create+write Module
instance ToJSON LocatedModule
instance FromJSON LocatedModule


-- | Witness that this script has module written in
data LocatedScript = LocatedScript [PathSegment] Script deriving Generic
instance ToJSON LocatedScript
instance FromJSON LocatedScript

newtype LocatedUserModule = LocatedUserModule
  { getLocatedUserModule :: LocatedModule }
newtype LocatedMainModule = LocatedMainModule
  { getLocatedMainModule :: LocatedModule }
newtype LocatedTestModule = LocatedTestModule
  { getLocatedTestModule :: LocatedModule }
  

newtype Script = Script { getScript :: T.Text } deriving (Show, Generic)
instance ToJSON Script
instance FromJSON Script
instance Semigroup Script where
  Script a <> Script b = Script $ a <> b
instance Monoid Script where
  mempty = Script mempty

newtype Symbol = Symbol { getSymbol :: T.Text } -- should never get a show instance
