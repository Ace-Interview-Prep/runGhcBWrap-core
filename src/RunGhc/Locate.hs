{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.Locate where

import Data.Aeson
import GHC.Generics
import System.FilePath
import qualified Data.List.Split as List
import qualified Data.Text as T

toSimpleImport :: [PathSegment] -> Import
toSimpleImport = Import Nothing 

toQualifiedImport :: T.Text -> [PathSegment] -> Import
toQualifiedImport qname segs = Import (Just qname) segs

newtype PathSegment = PathSegment { getPathSegment :: T.Text }
  deriving (Show, Generic)
instance ToJSON PathSegment
instance FromJSON PathSegment
data Import = Import
  { _import_qualifiedName :: Maybe T.Text
  , _import_pathSeg :: [PathSegment]
  } deriving (Show, Generic)
instance ToJSON Import
instance FromJSON Import

newtype Imports = Imports { getImports :: [Import] } deriving (Show, Generic)
instance FromJSON Imports
instance ToJSON Imports
instance Semigroup Imports where
  Imports a <> Imports b = Imports $ a <> b
instance Monoid Imports where
  mempty = Imports mempty

type FileExt = T.Text
pathSegsToPath :: FileExt -> [PathSegment] -> FilePath
pathSegsToPath fileExt ((PathSegment p):[]) = T.unpack $ p <> fileExt
pathSegsToPath fE ((PathSegment p):ps) = T.unpack p </> pathSegsToPath fE ps

pathSegsFromFilePath :: FilePath -> [PathSegment]
pathSegsFromFilePath = fmap (PathSegment . T.pack) . List.splitOn "/"

pathSegsToModuleName :: [PathSegment] -> T.Text
pathSegsToModuleName ((PathSegment p):[]) = p
pathSegsToModuleName ((PathSegment p):ps) = p <> "." <> pathSegsToModuleName ps

-- HeadlessScript ==> LocatedModule 
makeSourcePath :: FilePath -> [PathSegment] -> FilePath
makeSourcePath baseDir segs =
  let segs_ = T.intercalate "/" $ getPathSegment <$> segs
  in
    baseDir </> (T.unpack segs_)

showImportLine :: Import -> T.Text
showImportLine imp = case _import_qualifiedName imp of
  Nothing ->
    "import " <> pathSegsToModuleName (_import_pathSeg imp) <> ""
  Just qName ->
    "import qualified "
    <> pathSegsToModuleName (_import_pathSeg imp)
    <> " as "
    <> qName
    <> ""
