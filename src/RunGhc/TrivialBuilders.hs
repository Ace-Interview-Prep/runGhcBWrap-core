{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module RunGhc.TrivialBuilders where

import RunGhc.LocatedModule
import RunGhc.Locate
import RunGhc.SystemModule
import RunGhc.MakeExe
import RunGhc.Executable
import RunGhc.UserInput
import Data.Default
import qualified Data.Text as T



withF
  :: FunctionName
  -> (FunctionName -> Script)
  -> T.Text
  -> Either T.Text Executable
withF fname mkScript userScript = 
  Right $ toExe userModule testScript
  where
    userModule = mkUserModule' settings_ userScript
    testScript = LocatedTestModule $ locate [PathSegment "Main"] $ mkScript fname 
    settings_ = def
runF :: FunctionName -> T.Text -> Either T.Text Executable
runF fname userScript =
  Right $ toExe userModule testScript
  where
    userModule = mkUserModule' settings_ userScript
    testScript = simpleExeFromFunctionNameAsLib fname (getLocatedUserModule userModule)
    settings_ = def
  
runMain :: T.Text -> Either T.Text Executable
runMain = runF "main"

withSymbols
  :: Qualifiable args
  => args
  -> (args -> Script)
  -> T.Text
  -> Either T.Text Executable
withSymbols symbols mkScript userScript = 
  Right $ toExe userModule testScript
  where
    userModule = mkUserModule' settings_ userScript
    testScript = LocatedTestModule $ locate [PathSegment "Main"] $ mkScript symbols 
    settings_ = def

mkSelfTestedExecutable :: LocatedUserModule -> Executable
mkSelfTestedExecutable (LocatedUserModule loc) = mkExecutableNoLibrary loc


mkExecutableNoLibrary :: LocatedModule -> Executable
mkExecutableNoLibrary loc = Executable loc []

mkUserExecutableNoLibrary :: LocatedUserModule -> Executable
mkUserExecutableNoLibrary (LocatedUserModule loc) = Executable loc []

addUserLibraryToExecutableQualified :: T.Text -> LocatedUserModule -> Executable -> Executable
addUserLibraryToExecutableQualified qname (LocatedUserModule loc) exe =
  Executable
  (addImports (Imports [toQualifiedImport qname $ getPathSegments loc]) (_main exe))
  [loc]

-- Assumes user script at UserModule.hs and testing script at Main.hs
-- This is useful as sometimes we want to allow the user and tester
-- to write arbitrary scripts. Although we as the tester, should heavily rely on the Expression builders
unsafeMkExecutable :: Script -> Script -> Executable
unsafeMkExecutable userScript testScript =
  Executable
  (FromLocatedScript (LocatedScript [PathSegment "Main"] testScript))
  [(FromLocatedScript (LocatedScript [PathSegment "UserModule"] userScript))
  ]
--unsafeMkExecutable = 
newtype ImportName = ImportName T.Text

mkSimpleExecutable
  :: ImportName
  -> LocatedUserModule
  -> LocatedMainModule
  -> Executable
mkSimpleExecutable (ImportName qname) (locUser) (LocatedMainModule locMain) =
  addUserLibraryToExecutableQualified qname locUser $ mkExecutableNoLibrary locMain

asIs :: Script -> Executable
asIs scr =
  Executable
  { _main = FromLocatedScript $ LocatedScript [PathSegment "Main"] scr
  , _library = []
  }

-- There is no option for the user
-- to submit a partial file, and main is assumed to exist
asLibWithMain :: T.Text -> (LocatedUserModule, Bool)
asLibWithMain txt =
  (,True) -- that main exists
  $ LocatedUserModule
  $ locate [PathSegment "UserLibrary"]
  $ Script txt

-- There is no option for the user
-- to submit a partial file, and main is assumed to exist
asExpressionsWithMain :: T.Text -> (LocatedUserModule, Bool)
asExpressionsWithMain txt =
  (,True) -- that main exists
  $ LocatedUserModule
  $ locate [PathSegment "UserLibrary"]
  $ ExpressionsOnly $ Expressions txt

mkCommonExecutable :: CommonExe -> Executable
mkCommonExecutable = undefined

data CommonExe
  = SimpleUserScript Script -- is main, with maybe some actions
  -- ^ Run as main
  | TestedUserExpression Expressions SystemModule
  -- ^ User expression , our test
  | TestedUserScript Script SystemModule
  -- ^ User script , our test
  | TestedLocatedUserScript LocatedScript SystemModule
  -- ^ User located script , our test
  --- | forall a. Locatable a => Tested a SystemModule 
  | UserProject LocatedModule [LocatedModule]
  -- ^ Main , Library
  --- | TestedUserProject LocatedTestModule LocatedUserModule [LocatedModule]
  -- likely gonna be a special type of test we write but not necessarily 
  -- linkably different

--- Put in new module? ---------------------------------------------------------

