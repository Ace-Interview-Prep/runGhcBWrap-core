{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.Executable where

import RunGhc.SystemModule
import RunGhc.LocatedModule
import RunGhc.Locate
import System.FilePath
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- NOTE: we are going to need functionality to get from github
-- or at least from .tar.gz

data Executable = Executable
  { _main :: LocatedModule
  , _library :: [LocatedModule]
  } deriving Generic
instance ToJSON Executable
instance FromJSON Executable

data ExecutableWithDeps = ExecutableWithDeps
  { _exe :: Executable
  , _packages :: [PackageName]
  }

data ExecutableWithDepsWithCommand = ExecutableWithDepsWithCommand
  { _exeWithDeps :: ExecutableWithDeps
  , _commandWithArgs :: [T.Text] -- default would be runghc
  }

newtype PackageName = PackageName { getPackageName :: T.Text }

getTargetModule :: Executable -> [PathSegment]
getTargetModule = getPathSegments . _main

getTargetModulePath :: T.Text -> Executable -> FilePath
getTargetModulePath fext = pathSegsToPath fext  . getTargetModule 

-- Perhaps Executable becomes own module
writeExecutable :: FilePath -> Executable -> IO ()
writeExecutable baseDir exe =
  writeLocatedFiles baseDir $ _main exe : _library exe

writeLocatedFiles :: FilePath -> [LocatedModule] -> IO ()
writeLocatedFiles baseDir modules = do
  mapM_ (writeLocatedFile baseDir) modules
  where
    fileExt = ".hs"
    
    writeLocatedFile baseDir_ = \case
      FromLocatedScript (LocatedScript pathSegs (Script script)) -> 
        T.writeFile (baseDir_ </> pathSegsToPath fileExt pathSegs) script
      FromSystemModule pathSegs systemModule -> do
        T.writeFile (baseDir </> pathSegsToPath fileExt pathSegs) $ getScript $ systemModuleToScript pathSegs systemModule
        -- So that we can write to file
    makeModuleDeclaration :: [PathSegment] -> Script
    makeModuleDeclaration pathSegs = Script ("module " <> (pathSegsToModuleName pathSegs) <> " where\n")

    mkImport :: Import -> T.Text
    mkImport imp = case _import_qualifiedName imp of
      Nothing -> "import " <> pathSegsToModuleName (_import_pathSeg imp)
      Just qName -> "import qualified " <> pathSegsToModuleName (_import_pathSeg imp) <> " as " <> qName 
    makeImports (Imports importList) = Script $ T.unlines $ mkImport <$> importList

    fromExpressions :: Expressions -> Script
    fromExpressions = Script . getExpressions

    makeExtensions (Extensions (exts)) = Script $ 
      "{-# LANGUAGE" <> T.intercalate ", " exts <> " #-}"
    
    systemModuleToScript :: [PathSegment] -> SystemModule -> Script
    systemModuleToScript pathSegs = \case
      ExpressionsOnly (Expressions rawSource) ->
        makeModuleDeclaration pathSegs -- Script ("module " <> T.pack (pathSegsToModuleName pathSegs) <> " where\n")
        <> Script rawSource
      ExpressionsImportsOnly imports (Expressions rawSource) ->
        makeModuleDeclaration pathSegs
        <> makeImports imports
        <> Script rawSource
      ExpressionsImportsExtensionsOnly extensions imports (Expressions rawSource) ->
        makeExtensions extensions
        <> makeModuleDeclaration pathSegs
        <> makeImports imports
        <> Script rawSource

-- attachLibraryToUserModule :: LocatedUserModule -> Located-> [LocatedModule] -> Executable
-- attachLibraryToUserModule 
addUserLibraryToExecutable :: LocatedUserModule -> Executable -> Executable
addUserLibraryToExecutable (LocatedUserModule loc) exe = Executable (addImports (Imports [toSimpleImport $ getPathSegments loc]) (_main exe)) [loc]

addSystemLibraryToExecutable :: LocatedTestModule -> Executable -> Executable
addSystemLibraryToExecutable (LocatedTestModule locs) exe = Executable (_main exe) $ locs : (_library exe)

addSystemLibrariesToExecutable :: [LocatedModule] -> Executable -> Executable
addSystemLibrariesToExecutable locs exe = Executable (_main exe) $ locs <> (_library exe)

