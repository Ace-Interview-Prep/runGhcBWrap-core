{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module RunGhc.SystemModule where

import RunGhc.Locate
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

-- We locate at the latest possible step, for flexibility
--
-- NOTE that partial-modules written by the user are still
-- a SystemModule as we are the builder
data SystemModule
  = ExpressionsOnly Expressions
  | ExpressionsImportsOnly Imports Expressions
  | ExpressionsImportsExtensionsOnly Extensions Imports Expressions
  deriving Generic
  -- todo: as Semigroup
instance ToJSON SystemModule
instance FromJSON SystemModule

instance Semigroup SystemModule where
  a <> b =
    ExpressionsImportsExtensionsOnly
    (askExtensions a <> askExtensions b)
    (askImports a <> askImports b)
    (askExpressions a <> askExpressions b)
  --   -- case (a,b) of    
  --   -- (ExpressionsOnly
  -- ExpressionsOnly expr <> 
  -- | ExpressionsImportsOnly Imports Expressions
  -- | ExpressionsImportsExtensionsOnly Extensions Imports Expressions

type FunctionName = T.Text

askExpressions :: SystemModule -> Expressions
askExpressions = \case
  ExpressionsOnly e -> e
  ExpressionsImportsOnly _ e -> e
  ExpressionsImportsExtensionsOnly _ _ e -> e
askImports :: SystemModule -> Imports
askImports = \case
  ExpressionsOnly _ -> mempty
  ExpressionsImportsOnly i _ -> i
  ExpressionsImportsExtensionsOnly _ i _ -> i
askExtensions :: SystemModule -> Extensions
askExtensions = \case
  ExpressionsOnly _ -> mempty
  ExpressionsImportsOnly _ _ -> mempty
  ExpressionsImportsExtensionsOnly exts _ _ -> exts

-- showImportLine :: Import -> T.Text
-- showImportLine imp = case _import_qualifiedName imp of
--   Nothing ->
--     "import " <> pathSegsToModuleName (_import_pathSeg imp) <> "\n"
--   Just qName ->
--     "import qualified "
--     <> pathSegsToModuleName (_import_pathSeg imp)
--     <> " as "
--     <> qName
--     <> "\n"

class SemigroupModule a where
  addImports :: Imports -> a -> a
  addExtensions :: Extensions -> a -> a
  addFuncsDataDecls :: Expressions -> a -> a

instance SemigroupModule SystemModule where
  addImports = addImportsSystemModule
  addExtensions = addExtensionsSystemModule
  addFuncsDataDecls = addExpressionSource 

addExtensionsSystemModule :: Extensions -> SystemModule -> SystemModule
addExtensionsSystemModule exts = \case
  ExpressionsOnly expr ->
    ExpressionsImportsExtensionsOnly exts (Imports []) expr
  ExpressionsImportsOnly importList expr ->
    ExpressionsImportsExtensionsOnly exts importList expr
  ExpressionsImportsExtensionsOnly exts1 importList expr ->
    ExpressionsImportsExtensionsOnly (exts1 <> exts) importList expr
    
addImportsSystemModule :: Imports -> SystemModule -> SystemModule
addImportsSystemModule imports = \case
  ExpressionsOnly expr ->
    ExpressionsImportsOnly imports expr
  ExpressionsImportsOnly importList expr ->
    ExpressionsImportsOnly (imports <> importList) expr
  ExpressionsImportsExtensionsOnly extensions importList expr ->
    ExpressionsImportsExtensionsOnly extensions (imports <> importList) expr
  
    
addExpressionSource :: Expressions -> SystemModule -> SystemModule
addExpressionSource (Expressions txt2) = \case
  ExpressionsOnly (Expressions txt1) ->
    ExpressionsOnly (Expressions $ txt1 <> txt2)
  ExpressionsImportsOnly importList (Expressions txt1) ->
    ExpressionsImportsOnly importList (Expressions $ txt1 <> txt2)
  ExpressionsImportsExtensionsOnly extensions importList (Expressions txt1) ->
    ExpressionsImportsExtensionsOnly extensions importList (Expressions $ txt1 <> txt2)
    

newtype Expressions = Expressions { getExpressions :: T.Text }
  deriving (Show, Generic)
newtype Extensions = Extensions { getExtensions :: [T.Text] }
  deriving (Show, Generic)
instance ToJSON Expressions
instance FromJSON Expressions
instance ToJSON Extensions
instance FromJSON Extensions


instance Semigroup Extensions where
  Extensions a <> Extensions b = Extensions $ a <> b
instance Monoid Extensions where
  mempty = Extensions mempty
instance Semigroup Expressions where
  Expressions a <> Expressions b = Expressions $ a <> b

