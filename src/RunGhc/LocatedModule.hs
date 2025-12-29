{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module LocatedModule where

import System.FilePath
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- HeadlessScript ==> LocatedModule 
makeSourcePath :: FilePath -> [PathSegment] -> FilePath
makeSourcePath baseDir segs =
  let segs_ = T.intercalate "/" $ getPathSegment <$> segs
  in
    baseDir </> (T.unpack segs_)

writeExecutable :: FilePath -> Executable -> IO ()
writeExecutable baseDir exe =
  writeLocatedFiles baseDir $ _main exe : _library exe

writeLocatedFiles :: FilePath -> [LocatedModule] -> IO ()
writeLocatedFiles baseDir modules = do
  mapM_ (writeLocatedFile baseDir) modules
  where
    writeLocatedFile baseDir_ = \case
      FromLocatedScript (LocatedScript pathSegs (Script script)) -> 
        T.writeFile (baseDir_ </> pathSegsToPath pathSegs) script
      FromSystemModule pathSegs systemModule -> do
        T.writeFile (baseDir </> pathSegsToPath pathSegs) $ getScript $ systemModuleToScript pathSegs systemModule
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
        

pathSegsToPath :: [PathSegment] -> FilePath
pathSegsToPath ((PathSegment p):[]) = T.unpack p
pathSegsToPath ((PathSegment p):ps) = T.unpack p </> pathSegsToPath ps

pathSegsToModuleName :: [PathSegment] -> T.Text
pathSegsToModuleName ((PathSegment p):[]) = p
pathSegsToModuleName ((PathSegment p):ps) = p <> "." <> pathSegsToModuleName ps

class SemigroupModule a where
  addImports :: Imports -> a -> a
  addExtensions :: Extensions -> a -> a
  addFuncsDataDecls :: Expressions -> a -> a

instance SemigroupModule SystemModule where
  addImports = addImportsSystemModule
  addExtensions = addExtensionsSystemModule
  addFuncsDataDecls = addExpressionSource 

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
    

-- NOTE: we are going to need functionality to get from github
-- or at least from .tar.gz

data Executable = Executable
  { _main :: LocatedModule
  , _library :: [LocatedModule]
  }

data ExecutableWithDeps = ExecutableWithDeps
  { _exe :: Executable
  , _packages :: [PackageName]
  }

data ExecutableWithDepsWithCommand = ExecutableWithDepsWithCommand
  { _exeWithDeps :: ExecutableWithDeps
  , _commandWithArgs :: [T.Text] -- default would be runghc
  }

newtype PackageName = PackageName { getPackageName :: T.Text }

getPathSegments :: LocatedModule -> [PathSegment]
getPathSegments = \case
  FromLocatedScript (LocatedScript pathSegs _) -> pathSegs
  FromSystemModule pathSegs _ -> pathSegs

getTargetModule :: Executable -> [PathSegment]
getTargetModule = getPathSegments . _main

getTargetModulePath :: Executable -> FilePath
getTargetModulePath = pathSegsToPath . getTargetModule 

data DependentLocatedModule = DependentLocatedModule
  { _locatedModule :: LocatedModule
  , _deps :: [[PathSegment]]
  }

data LocatedModule
  = FromLocatedScript LocatedScript
  | FromSystemModule [PathSegment] SystemModule

newtype LocatedUserModule = LocatedUserModule LocatedModule
newtype LocatedMainModule = LocatedMainModule LocatedModule
  
newtype PathSegment = PathSegment { getPathSegment :: T.Text } deriving Show

-- | Witness that this script has module written in
data LocatedScript = LocatedScript [PathSegment] Script

-- We locate at the latest possible step, for flexibility
--
-- NOTE that partial-modules written by the user are still
-- a SystemModule as we are the builder
data SystemModule
  = ExpressionsOnly Expressions
  | ExpressionsImportsOnly Imports Expressions
  | ExpressionsImportsExtensionsOnly Extensions Imports Expressions
  -- todo: as Semigroup
  
newtype Expressions = Expressions { getExpressions :: T.Text } deriving Show
newtype Extensions = Extensions { getExtensions :: [T.Text] } deriving Show
newtype Imports = Imports { getImports :: [Import] } deriving Show

instance Semigroup Extensions where
  Extensions a <> Extensions b = Extensions $ a <> b 
instance Semigroup Imports where
  Imports a <> Imports b = Imports $ a <> b
-- eg Extensions [ "OverloadededStrings" ]i
instance Semigroup Script where
  Script a <> Script b = Script $ a <> b
instance Monoid Script where
  mempty = Script mempty

data Import = Import
  { _import_qualifiedName :: Maybe T.Text
  , _import_pathSeg :: [PathSegment]
  } deriving Show

newtype Script = Script { getScript :: T.Text } deriving Show

