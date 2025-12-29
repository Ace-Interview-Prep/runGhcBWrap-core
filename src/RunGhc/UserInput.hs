{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module UserInput where

{-
A collection of helper functions for providing something our API can understand which is ultimately just the Executable type
-}

import Scrappy.Scrape
import Text.Parsec
import LocatedModule
import qualified Data.Text as T
import qualified Data.List.Split as List
import Data.Char (toUpper, toLower)


toPathSegs :: FilePath -> [PathSegment]
toPathSegs = fmap (PathSegment . T.pack) . List.splitOn "/"

-- attachLibraryToUserModule :: LocatedUserModule -> Located-> [LocatedModule] -> Executable
-- attachLibraryToUserModule 
addUserLibraryToExecutable :: LocatedUserModule -> Executable -> Executable
addUserLibraryToExecutable (LocatedUserModule loc) exe = Executable (addImports (Imports [toSimpleImport $ getPathSegments loc]) (_main exe)) [loc]

addUserLibraryToExecutableQualified :: T.Text -> LocatedUserModule -> Executable -> Executable
addUserLibraryToExecutableQualified qname (LocatedUserModule loc) exe = Executable (addImports (Imports [toQualifiedImport qname $ getPathSegments loc]) (_main exe)) [loc]

toSimpleImport :: [PathSegment] -> Import
toSimpleImport = Import Nothing 

toQualifiedImport :: T.Text -> [PathSegment] -> Import
toQualifiedImport qname segs = Import (Just qname) segs

addSystemLibraryToExecutable :: LocatedModule -> Executable -> Executable
addSystemLibraryToExecutable locs exe = Executable (_main exe) $ locs : (_library exe)

addSystemLibrariesToExecutable :: [LocatedModule] -> Executable -> Executable
addSystemLibrariesToExecutable locs exe = Executable (_main exe) $ locs <> (_library exe)

mkExecutableNoLibrary :: LocatedModule -> Executable
mkExecutableNoLibrary loc = Executable loc []

mkSelfTestedExecutable :: LocatedUserModule -> Executable
mkSelfTestedExecutable (LocatedUserModule loc) = mkExecutableNoLibrary loc


-- Assumes user script at UserModule.hs and testing script at Main.hs
-- This is useful as sometimes we want to allow the user and tester
-- to write arbitrary scripts. Although we as the tester, should heavily rely on the Expression builders
unsafeMkExecutable :: Script -> Script -> Executable
unsafeMkExecutable userScript testScript =
  Executable
  (FromLocatedScript (LocatedScript [PathSegment "Main.hs"] testScript))
  [(FromLocatedScript (LocatedScript [PathSegment "UserModule.hs"] userScript))
  ]
--unsafeMkExecutable = 
newtype ImportName = ImportName T.Text

mkSimpleExecutable :: ImportName -> LocatedUserModule -> LocatedMainModule -> Executable
mkSimpleExecutable (ImportName qname) (locUser) (LocatedMainModule locMain) =
  addUserLibraryToExecutableQualified qname locUser $ mkExecutableNoLibrary locMain

handleUserInputSingleFile :: T.Text -> LocatedUserModule
handleUserInputSingleFile txt =
  LocatedUserModule $ FromLocatedScript $ LocatedScript [PathSegment "UserLibrary"] $ Script txt

handleUserInputExpressionsOnly :: T.Text -> LocatedUserModule
handleUserInputExpressionsOnly txt =
  LocatedUserModule $ FromSystemModule [PathSegment "UserLibrary"] $ ExpressionsOnly $ Expressions txt

-- Note that on a LocatedModule we should always be able to apply
-- a function (f :: SystemModule -> SystemModule) and if it is actually
-- a LocatedScript then we do nothing
-- f `apply` lMod = lMod ; when lMod is lScript
tryHandleUserInputExpressionsOnly :: T.Text -> LocatedUserModule
tryHandleUserInputExpressionsOnly txt =
  let
    p = do
      try (string "module")
        <|> try (string "where")
        <|> string "import"
        <|> ( string "{-#"
              >> many space
              >> caseInsensitiveString "language"
            )
      -- or if there exists import
    x = scrape p $ T.unpack txt
    x' = case x of
      Nothing -> FromSystemModule [PathSegment "UserLibrary"] $ ExpressionsOnly $ Expressions txt  -- is as expected a function or set of functions and types
      Just ("module":"where":xs) ->
        FromLocatedScript
        $ LocatedScript [PathSegment "UserLibrary"] -- Here we constrain to only allowing UserLibrary.hs as Path position
        $ Script txt
        -- is full module
      Just xs | "import" `elem` xs ->
                undefined -- is full module but no module name
        
  in
    LocatedUserModule x' 
    --FromSystemModule ["UserLibrary"] $ ExpressionsOnly 

handleUserInputMultipleFiles :: (FilePath, Script) -> [(FilePath, T.Text)] -> Executable
handleUserInputMultipleFiles main@(targetPath, Script mainSrc) txtsWithPath =
  let
    f (fp, script) =
      FromLocatedScript $ LocatedScript (toPathSegs fp) $ Script script
    fMain (fp, Script script) =
      FromLocatedScript $ LocatedScript (toPathSegs fp) $ Script script
      
  in
    Executable (fMain main) (f <$> txtsWithPath)
    -- case targetPath `List.lookup` $ txtsWithPath of
    --   Nothing
    -- f <$> (main : txtsWithPath)
  --FromLocatedScript $ LocatedScript ["UserLibrary"] $ Script txt


--IDEA:
caseInsensitiveString
  :: forall s u m
  . Stream s m Char
  => String
  -> ParsecT s u m String
caseInsensitiveString s = do
  let
    f :: Stream s m Char => Char -> ParsecT s u m Char
    f chr = char (toUpper chr) <|> char (toLower chr)
    --fs = f <$> s
  mapM f s
