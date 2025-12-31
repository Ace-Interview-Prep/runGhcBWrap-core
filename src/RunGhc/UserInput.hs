{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module RunGhc.UserInput where

{-
A collection of helper functions for providing something our API can understand which is ultimately just the Executable type
-}

import Scrappy.Scrape
import Text.Parsec
import RunGhc.Executable
import RunGhc.LocatedModule
import RunGhc.Locate
import RunGhc.SystemModule
import Data.Bifunctor
import Text.IStr
import Data.Default
import qualified Data.Text as T
import Data.Char (toUpper, toLower)

data UserModuleSettings = UserModuleSettings
  { _location :: [PathSegment]
  , _allowImports :: Bool
  , _overrideMake_If_main_exists :: Bool
  , _mkSystemModule :: Expressions -> SystemModule
  }
instance Default UserModuleSettings where
  def = UserModuleSettings
    { _location = [PathSegment "UserModule"]
    , _allowImports = True
    , _overrideMake_If_main_exists = False
    , _mkSystemModule = ExpressionsOnly
    }

mkUserModule' :: UserModuleSettings -> T.Text -> LocatedUserModule
mkUserModule' cfg input =
  LocatedUserModule $ mkUserModule 
  (_location cfg)
  (_allowImports cfg)
  (_overrideMake_If_main_exists cfg)
  (_mkSystemModule cfg)
  input

handleUserInputMultipleFiles :: (FilePath, Script) -> [(FilePath, T.Text)] -> Executable
handleUserInputMultipleFiles main@(targetPath, Script mainSrc) txtsWithPath =
  let
    f (fp, script) =
      FromLocatedScript $ LocatedScript (pathSegsFromFilePath fp) $ Script script
    fMain (fp, Script script) =
      FromLocatedScript $ LocatedScript (pathSegsFromFilePath fp) $ Script script
  in
    Executable (fMain main) (f <$> txtsWithPath)

-- tryHandleUserInput 
parseUserScript
  :: T.Text -- Raw user input 
  -> Bool -- can contain imports?
  -> Bool -- can contain main?
  -> Either Script Expressions
parseUserScript txt canContainImports cancelWhenMainExists =  
  let
    moduleHeadPieces = findModuleHeadPieces txt
    mainExists = exists mainFuncParser $ T.unpack txt -- (string "main :: IO ()"
    x = case moduleHeadPieces of
      Just _ -> Left $ Script txt 
      Nothing -> Right $ Expressions txt

  in case moduleHeadPieces of
       Just _ ->
         -- We cannot interact with this script
         Left $ Script txt 
       Nothing ->
         case mainExists && cancelWhenMainExists of
           True -> Left $ Script txt
           False -> Right $ Expressions txt

handleUserInputSingleFile
  :: T.Text
  -> LocatedUserModule
handleUserInputSingleFile txt =
  LocatedUserModule
  $ FromLocatedScript
  $ LocatedScript [PathSegment "UserLibrary"]
  $ Script txt

    
-- We only build if they gave just an Expressions
unsafeMkSystemModuleFromExpressionsOnly
  :: Expressions
  -> Imports
  -> Extensions
  -> SystemModule
unsafeMkSystemModuleFromExpressionsOnly exprs imports extensions =
  -- LocatedUserModule
  -- $ FromSystemModuele [PathSegment "UserLibrary"]
  addExtensions extensions
  $ addImports imports 
  $ ExpressionsOnly
  $ exprs

-- Super naive function
unsafeMkLocatedUserModule
  :: Expressions
  -> LocatedUserModule
unsafeMkLocatedUserModule expr =
  LocatedUserModule
  $ FromSystemModule [PathSegment "UserLibrary"]
  $ ExpressionsOnly
  $ expr

(<+>) :: Applicative m => m a -> m b -> m (a, b)
ma <+> mb = (,) <$> ma <*> mb


-- 99% of cases are
-- Text -> LocatedUserModule
-- 


-- We should also have functionality to check if the Module exports main

-- case exportsMain { True -> Exe ; False -> Library }
-- ;
-- A general theme i am noticing is that if we want user freedom -> their code is Main.main (eg user playground/ first program)
-- but if we need to give any feedback or interaction at all, then their code
-- must be under <UserModuleName>.* (eg. Hackerrank style, large projects, chapter exercises)
--
--   Otherwise, even if they give main we should still write Main.hs ourselves. Why?
--       - We may need to affect the environment, which is actually more likely if they give `main :: IO ()`

-- We either have just an expression or something more
-- but either way we are trying to normalize it

-- Note that on a LocatedModule we should always be able to apply
-- a function (f :: SystemModule -> SystemModule) and if it is actually
-- a LocatedScript then we do nothing
-- f `apply` lMod = lMod ; when lMod is lScript
-- tryHandleUserInputExpressionsOnly
--   :: T.Text
--   -- ^ User Input direct
--   -> Imports
--   -- ^ Configured imports
--   -> Extensions
--   -- ^ ConfiguredExtensions
--   -> (LocatedUserModule, Bool) -- or is this unlocated???
-- tryHandleUserInputExpressionsOnly txt imports extensions =
--   let
--     locate_ :: forall a. Locatable a => a -> LocatedModule
--     locate_ = locate [PathSegment "UserLibrary"]
--     moduleHeadPieces = findModuleHeadPieces txt
--     mainExists = exists mainFuncParser $ T.unpack txt -- (string "main :: IO ()"
--   in
--     (,mainExists)
--     $ LocatedUserModule
--     $ case moduleHeadPieces of
--       Just ("module":"where":xs) -> locate_ $ Script txt
--       Just xs | "import" `elem` xs -> locate_ $ Script txt
--       Just something -> locate_ $ Script txt
--       Nothing ->
--         locate_ 
--         $ addExtensions extensions
--         $ addImports imports
--         $ ExpressionsOnly $ Expressions txt
        -- is as expected a function or set of functions and types
    --FromSystemModule ["UserLibrary"] $ ExpressionsOnly 

-- the theoretical reason why existence of main, may matter is that
-- in some cases, we want to allow that to override the mkExe logic
--
-- In those cases, if `main` exists then we should (give option to) avoid mkModule
  -- this would most likely be when main is not in Symbols expected by Test script

-- ( importsExist @Bool -> mainExists @Bool -> choice_runMkModule @Bool )
  -- implicitly the existence of Main or Extensions |absolutely| negates
  -- any user choices

-- x :: LocatedModule
-- x =
--   -- either id id
--   -- $ bimap
--   -- (locate pathSegs)
--   fromLocatableSource [PathSegment "UserModule"]
--   $
--   (fmap mkLocatedUserModule :: SystemModule -> SystemModule) --LocatedModule)    
--   $ tryHandleUserInput "input"


--f :: Bool -> Bool -> (SystemModule -> SystemModule) -> T.Text -> LocatedModule

--TODO: system module 

-- wouldnt it always be an Expressions type?
  
-- TODO: give module name as string arg
mkUserModule
  :: [PathSegment] -- location
  -> Bool -- allow imports?
  -> Bool -- main exists?
  -> (Expressions -> SystemModule) -- mkModule (eg addImports, addExts, ...)
  -> T.Text -- User input
  -> LocatedModule
mkUserModule pathSegs allowImports mainExists mutModule input = 
  locateUserSource pathSegs
  $ fmap mutModule
  $ parseUserScript input allowImports mainExists 

-- Handle if user wrote their own full module,
-- or if we can make a SystemModule freely
locateUserSource
  :: [PathSegment]
  -> Either Script SystemModule
  -> LocatedModule
locateUserSource pathSegs userScript =
  either id id
  $ bimap
  (locate pathSegs) 
  (locate pathSegs) 
  userScript
          
    -- case targetPath `List.lookup` $ txtsWithPath of
    --   Nothing
    -- f <$> (main : txtsWithPath)
  --FromLocatedScript $ LocatedScript ["UserLibrary"] $ Script txt


--- Parsers used for scraping + analysis

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


mainFuncParser :: Stream s m Char => ParsecT s u m ()
mainFuncParser = try a <|> b
  where
    a = do
      _ <- string "main"
      _ <- many space
      _ <- string "::"
      _ <- many space
      _ <- string "IO"
      _ <- many space
      _ <- string "()"
      pure ()
    b = do
      _ <- string "main"
      _ <- many space
      _ <- string "="
      pure ()
      
  
findModuleHeadPieces :: T.Text -> Maybe [String]
findModuleHeadPieces txt = scrape moduleHeadPiece $ T.unpack txt
    
moduleHeadPiece :: Stream s m Char => ParsecT s u m String
moduleHeadPiece = do
  try (string "module")
    <|> try (string "where")
    <|> string "import"
    <|> ( string "{-#"
          >> many space
          >> caseInsensitiveString "language"
        )
  -- or if there exists import



  
  -- \case
  -- _

-- -- attachLibraryToUserModule :: LocatedUserModule -> Located-> [LocatedModule] -> Executable
-- -- attachLibraryToUserModule 
-- addUserLibraryToExecutable :: LocatedUserModule -> Executable -> Executable
-- addUserLibraryToExecutable (LocatedUserModule loc) exe = Executable (addImports (Imports [toSimpleImport $ getPathSegments loc]) (_main exe)) [loc]


-- addSystemLibraryToExecutable :: LocatedTestModule -> Executable -> Executable
-- addSystemLibraryToExecutable locs exe = Executable (_main exe) $ locs : (_library exe)

-- mkUserExecutableNoLibrary :: LocatedUserModule -> Executable
-- mkUserExecutableNoLibrary (LocatedUserModule loc) = Executable loc []
-- run the user script 'as is' with zero processing

-- we want the writer of the script to pass the qualified name
      
-- test_1Input :: FunctionName -> Expression -> Expressions
-- test_1Input fname (Expression expr)  = Expressions $ fname <> " " <> expr

-- test_card1 :: FunctionName -> Expression -> Expressions

-- test_card2 :: FunctionName -> Expression -> Expression -> Expressions

-- test_card2 :: FunctionName -> Expression -> Expression -> Expressions

  
-- testThisFunction
--   :: FunctionName
--   -> Expression
--   -> Executable



-- fromDependentSystemModule
--   :: Qualifiable a
--   => [PathSegment]
--   -> Imports
--   -> Extensions
--   -> a -- FunctionName for instance
--   -> LocatedModule -- The user module
--   -> SystemModule
--   -> LocatedModule
-- fromDependentSystemModule pathLoc imps exts symbols userModule sysModule = 
--   addExtensions exts
--   $ addImports (Imports [localQualifiedImport "UserModule" userModule])
--   $ addImports imps 
--   $ FromSystemModule pathLoc sysModule  
--
  ---  $ ExpressionsOnly $ withF (qualify "UserModule" symbols)
-- fromDependentSystemModule'
--   :: Qualifiable a
--   => [PathSegment]
--   -> a -- FunctionName for instance
--   -> LocatedModule -- The user module
--   -> SystemModule
--   -> LocatedModule
-- fromDependentSystemModule' pathLoc symbols userModule sysModule = 
--   addImports (Imports [localQualifiedImport "UserModule" userModule])
--   $ FromSystemModule pathLoc sysModule  
-- | TODO:
-- 
--   instead this adding of localQualifiedImport
--   should be during a system module creation
--

  


-- let secondLine =
--       if "main :: IO ()" `isInfixOf` userMod
--       then \f -> f -- "#{f}"
--       else \f -> "mapM_ (print . " <> f <> ") $ toColor [1..10]"
-- (\f -> [istr|
-- main :: IO ()
-- main = do
--   setupEnv
--   (#{secondLine hasMain f}) 
--   --mapM_ (print . #{f}) $ toColor [1..10]
--   runChecks
  

-- data Color = Red | Blue | Green | Orange | Violet | Black | Gold deriving (Eq, Ord, Enum)

-- toColor = fromEnum
-- |]
  
-- )




-- chooseTestScriptBuild
--   :: (LocatedUserModule, Bool)
--   -> (LocatedUserModule -> symbols -> (symbols -> Expressions))
--   -> Expressions 
--   -> LocatedTestModule
-- chooseTestScriptBuild (userModule, hasMain) whenNoMain whenMain = undefined


  
    
-- setup
-- run Either Main (MkScript :: symbols -> Expressions)
-- Value-based tests
  

  -- mkMainExeWithDefaultHead user "main" $ \main_ ->
  -- Expressions $ [istr| main = #{main_} |]


--   if they gave a UserModule.main :: IO () , specifically "main :: IO ()" exists
--   then write the environment? with bracket

-- bracket f (setupEnv) (reportBenchmarks)


-- bracket UserModule.main setupEnv (runEqualityChecks <+> runBenchmarks)

  
  -- makeTestScriptWithImportsExtensions
  -- [PathSegment "Main"]
  -- (fst $ defaultHeadMainModule)
  -- (snd $ defaultHeadMainModule)
  -- symbols
  -- userModule
  -- mkExpr

  
-- mk userModule = makeTestScriptWithImportsExtensions
--   [PathSegment "Main"]
--   mempty
--   mempty
--   (Symbol "f", Symbol "g", Symbol "Maybee")
--   userModule
--   (\(Symbol fname, Symbol gname, Symbol typeName) -> [istr|main = #{gname} @#{typeName} $ #{fname} <$> [1..10]|])

-- mk2 userModule = makeTestScriptWithImportsExtensions
--   [PathSegment "Main"]
--   (Imports [ Import (Just "T") ["Data", "Text"]
--            , Import (Just "Json") ["Data", "Aeson"]
--            ])
--   (Extensions ["OverloadedStrings"])
--   "f"
--   -- In theory we could pass a reftype
--   userModule
--   (\fname -> [istr|
-- main = #{fname} <$> (fmap T.show [1..10])
-- |] <> dataTypeExists "Maybee")


-- makeTestScriptWithDefaultHead_1ref
--   :: LocatedModule -- The user module
--   -> Symbol
--   -> (FunctionName -> Expressions)
--   -> LocatedModule
-- makeTestScriptWithDefaultHead userModule fname symbol mkExpr =

-- makeTestScriptWithDefaultHead_1ref
--   :: LocatedModule -- The user module
--   -> Symbol
--   -> (FunctionName -> Expressions)
--   -> LocatedModule
-- makeTestScriptWithDefaultHead_1ref userModule fname symbol mkExpr =
 
-- makeTestScriptWithDefaultHead_2ref
--   :: LocatedModule -- The user module
--   -> FunctionName
--   -> (Symbol, Symbol)
--   -> ((Symbol, Symbol) -> Expressions)
--   -> LocatedModule
-- makeTestScriptWithDefaultHead_2ref userModule (symbol1, symbol2) mkExpr =
--   let (imports, exts) = defaultHeadMainModule
--   in makeTestScriptWithImportsExtensions
--      [PathSegment "Main"]
--      imports
--      exts
--      (symbol1, symbol2)
--      userModule
--      mkExpr
     
-- -- [istr|

-- |]
-- [istr|
-- module Main where
-- #{importLine}
-- main = #{qname}.#{fname}
-- |]

  
  
-- mkUserExecutableAsLibrary
--   :: LocatedUserModule
--   -> Executable
-- mkUserExecutableAsLibrary (LocatedUserModule loc) = Executable (mod qname_) [loc]
--   where
--     qname_ = "UserModule"
--     importLine = showImportLine $ localQualifiedImport qname loc --Import Nothing [getPathSegments loc
--     mod qname = FromLocatedScript $ LocatedScript [PathSegment "Main"] $ Script    
--       --""
--       $ [istr|
-- module Main where
-- #{importLine}
-- main = #{qname}.main
         
--          |]
