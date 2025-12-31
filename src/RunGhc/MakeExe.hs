{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module RunGhc.MakeExe where

import RunGhc.Locate
import RunGhc.LocatedModule
import RunGhc.Executable
import RunGhc.SystemModule

import Text.IStr
import qualified Data.List as List
import qualified Data.Text as T

-- simple tests that are monoidal and evaluate the validity of user input

-- PLAN
-- build user module somehow
  -- `tryHandleUserInputExpressionsOnly` OR `asLibWithMain`
-- build test script with reference to user module
  -- `mkTestScriptBracketed`
  --
  -- OR any other way :: (T.Text -> Executable)
  --   but more specifically (T.Text -> (LocatedUserModule, LocatedTestModule))
-- we locate our test script at Main
-- we morph into executable


-- NOTE:
-- (LocatedUserModule, LocatedTestModule)
-- is isomorphic to Executable
toExe :: LocatedUserModule -> LocatedTestModule -> Executable
toExe (LocatedUserModule locUser) (LocatedTestModule locTest) =
  Executable
  { _main = locTest
  , _library = [locUser]
  }

defaultHeadMainModule :: (Imports, Extensions)
defaultHeadMainModule =
  ( (Imports
      [ Import Nothing $ PathSegment <$> ["Data", "Text"]
      , Import Nothing $ PathSegment <$> ["Data", "Aeson"]
      ])
  , (Extensions
      ["OverloadedStrings"
      ])
  )

-- Cuter way to do it
-- The symbol represents a name the script must provide
mkTestScriptBracketed
  :: Qualifiable symbolic
  => LocatedUserModule
  -> symbolic
  -> Imports
  -> Extensions
  -> (Symbol -> Expressions) -- before
  -> (Symbol -> Expressions) -- after 
  -> (Symbol -> symbolic -> Expressions)
  -- ^ make test function
  -- first Symbol is just to define main properly  
  -> SystemModule --LocatedTestModule
mkTestScriptBracketed (userModule) symbols imports extensions before after mkTest =
  -- We can also easily call Locate on this.. at any point
  ExpressionsImportsExtensionsOnly
  extensions
  imports
  $ scriptMkMain
  <> (before pre)
  <> (mkTest inner symbols) -- (if hasMain then ")--(mkTest inner)
  <> (after post)
  where
    pre = Symbol "pre"
    inner = Symbol "inner"
    post = Symbol "post"
    scriptMkMain = Expressions $ T.pack x
    x :: String    
    x = [istr|
main :: IO ()
main = #{getSymbol pre} >> #{getSymbol inner} >> #{getSymbol post} 
|]


-- makeTestScript "f" withF
makeTestScript :: [PathSegment] -> FunctionName -> (FunctionName -> Expressions) -> LocatedModule
makeTestScript pathLoc fname withF = FromSystemModule pathLoc $ ExpressionsOnly $ withF fname

type QualifiedName = T.Text
makeTestScriptWithImportsExtensions
  :: Qualifiable a
  => [PathSegment]
  -> Imports
  -> Extensions
  -> a -- FunctionName for instance
  -> LocatedModule -- The user module
  -> (a -> Expressions)
  -> LocatedModule
makeTestScriptWithImportsExtensions pathLoc imps exts symbols userModule withF =
  addExtensions exts
  $ addImports (Imports [localQualifiedImport "UserModule" userModule])
  $ addImports imps $ FromSystemModule pathLoc $ ExpressionsOnly $ withF (qualify "UserModule" symbols)



-- eg
mkMainWithDefaultHead
  :: Qualifiable symbols
  => LocatedModule
  -- ^ User module we interface with
  -> symbols
  -- ^ Symbols assumed to exist in UserModule
  -> (symbols -> Expressions)
  -- ^ Make Test
  -> LocatedModule
  -- ^ Resulting module
mkMainWithDefaultHead userModule symbols mkExpr =
  makeTestScriptWithImportsExtensions
  [PathSegment "Main"]
  (fst $ defaultHeadMainModule)
  (snd $ defaultHeadMainModule)
  symbols
  userModule
  mkExpr

simpleMainModule :: FunctionName -> LocatedUserModule -> LocatedTestModule
simpleMainModule fname (LocatedUserModule userMod) =
  simpleExeFromFunctionNameAsLib fname userMod
  --mkMainWithDefaultHead userModule symbols mkExpr
  
simpleExeFromFunctionNameAsLib :: FunctionName -> LocatedModule -> LocatedTestModule
simpleExeFromFunctionNameAsLib fname userMod = LocatedTestModule $ mkMainWithDefaultHead userMod (Symbol fname) $ \(Symbol func_) ->
  Expressions . T.pack $ [istr| main = #{func_} |]

-- This has a default module head as well as the import from UserModule
simpleExeFromUserMainAsLib :: LocatedModule -> LocatedTestModule
simpleExeFromUserMainAsLib userMod = simpleExeFromFunctionNameAsLib "main" userMod

-- SAME AS ABOVE
-- mkUserExecutableAsLibrary :: LocatedUserModule -> Executable
-- mkUserExecutableAsLibrary (LocatedUserModule loc) = Executable (mod qname_) [loc]
--   where
--     qname_ = "UserModule"
--     importLine = showImportLine $ localQualifiedImport qname_ loc --Import Nothing [getPathSegments loc
--     mod qname = FromLocatedScript $ LocatedScript [PathSegment "Main"] $ Script . T.pack $ [istr|
-- module Main where                                                                                       
-- #{importLine}
-- main = #{qname}.main
-- |]



mkMainExeWithDefaultHead
  :: Qualifiable symbols
  => LocatedModule
  -- ^ User module we interface with
  -> symbols
  -- ^ Symbols assumed to exist in UserModule
  -> (symbols -> Expressions)
  -- ^ Make Test
  -> Executable -- LocatedModule
  -- ^ Resulting module
mkMainExeWithDefaultHead userModule symbols mkExpr =
  Executable
  { _main = mkMainWithDefaultHead userModule symbols mkExpr
  , _library = [userModule]
  }


-- | callFunction "f" (LocatedModule{..})
-- where f is implicitly :: IO ()
callFunction'
  :: FunctionName
  -> LocatedUserModule
  -> (FunctionName -> Expressions)
  -> Executable
callFunction' fname (LocatedUserModule loc) mkExpr = Executable mod [loc]
  where
    qname = "UserModule"
    importLine = showImportLine $ localQualifiedImport qname loc --Import Nothing [getPathSegments loc
    toScript = Script . getExpressions 
    mod = FromLocatedScript $ LocatedScript [PathSegment "Main"] $ toScript $ mkExpr fname --Script    

  
class Qualifiable a where
  qualify :: T.Text -> a -> a
instance Qualifiable Symbol where
  qualify pre (Symbol sname) = Symbol $ pre <> "." <> sname
instance Qualifiable (Symbol, Symbol) where
  qualify pre (s1,s2) = (qualify pre s1, qualify pre s2)
instance Qualifiable (Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3) = (qualify pre s1, qualify pre s2, qualify pre s3)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3, s4) = (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3,s4,s5) = (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4, qualify pre s5)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3,s4,s5,s6) = (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4, qualify pre s5, qualify pre s6)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3,s4,s5,s6,s7) = (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4, qualify pre s5, qualify pre s6, qualify pre s7)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3,s4,s5,s6,s7,s8) = (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4, qualify pre s5, qualify pre s6, qualify pre s7, qualify pre s8)
instance Qualifiable (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol) where
  qualify pre (s1,s2,s3,s4,s5,s6,s7,s8,s9) =
    (qualify pre s1, qualify pre s2, qualify pre s3, qualify pre s4, qualify pre s5, qualify pre s6, qualify pre s7, qualify pre s8, qualify pre s9)
    


-- NOTE: for generating an infinite stream, since we still do (Take n)
-- we can always write this to input.json (and maybe other places) so that
-- the user can do whatever they want
--
-- Could also be that we have inputs/1.json , inputs/2.json .. inputs/n.json


type TypeName = T.Text
-- type FunctionName = T.Text
type DataConstructorName = T.Text
--
newtype Hash = Hash T.Text
data Function_ = Function_
  { _function_name :: T.Text
  , _function_typeSig :: TypeSig
  }
newtype ModuleName = ModuleName T.Text
-- newtype Imports = Imports [T.Text]
newtype TypeSig = TypeSig T.Text

data SumTypeInfo = SumTypeInfo
  { _sumTypeInfo_typeName :: T.Text
  , _sumTypeInfo_kind :: Int
  , _sumTypeInfo_expressions :: [SumTypeCase]
  }

data SumTypeCase = SumTypeCase
  { _sumTypeCase_name :: T.Text
  , _sumTypeCase_argsCount :: Int
  }

mkSumTypePattern :: SumTypeCase -> T.Text
mkSumTypePattern (SumTypeCase name argsCount) =
  T.pack $ '\t' : (T.unpack $ name <> " " <> (T.pack $ List.intersperse ' ' $ take argsCount $ repeat '_'))---['a' .. 'z']))

existsSumType :: SumTypeInfo -> Script
existsSumType sumInfo =
  let
    sumTypeName :: T.Text
    sumTypeName = _sumTypeInfo_typeName sumInfo

    fakeTypeArgs :: [Char]
    fakeTypeArgs = List.intersperse ' ' $ take (_sumTypeInfo_kind sumInfo) ['a' .. 'z']
    typeInTypeSig = sumTypeName <> " " <> T.pack fakeTypeArgs 
    patterns = fmap mkSumTypePattern $ _sumTypeInfo_expressions sumInfo
    
  in Script $ (T.pack $ [istr|
ace_required_CaseStatement_f#{sumTypeName} :: #{typeInTypeSig} -> Int
ace_required_CaseStatement_f#{sumTypeName} = \\case
|]) <> (T.unlines
       $ (\(list_)  -> list_ <> ["\t_ -> -1"])
       $ fmap (\(num, pattern_) -> (pattern_) <> " -> " <> (T.pack $ show @Int num) )
       $ zip
       [1..]
       $
       patterns 
      )

existsType' :: Hash -> TypeName -> Script
existsType' (Hash h) t = Script $ T.pack [istr|
ace_requiredType_T_#{h} :: #{t}
ace_requiredType_T_#{h} = undefined
|]

data FunctionInfo = FunctionInfo
  { _functionInfo_name :: T.Text
  , _functionInfo_typeSig :: T.Text
  }

-- Test both 
existsFunction'' :: FunctionInfo -> Script
existsFunction'' fInfo =
  let
    fName = _functionInfo_name fInfo
    tSig = _functionInfo_typeSig fInfo
    
  in Script $ T.pack    
     [istr|
ace_requiredFunctionInfo_F_#{fName} :: #{tSig}
ace_requiredFunctionInfo_F_#{fName} = #{fName} 

ace_requiredFunctionInfo_F_#{fName}_Copy = ace_requiredFunctionInfo_F_#{fName}       
       |]

existsFunctionName' :: FunctionName -> Script
existsFunctionName' fName = Script $ T.pack [istr|
ace_requiredFunction_F_#{fName} = #{fName} 
|]

data ClassInfo = ClassInfo
  { _clsInfo_className :: T.Text
  , _clsInfo_functionHeads :: [(FunctionName, TypeSig)]
  }

-- Should type sig be a little more specific?
-- data TypeSig' = TypeSig'
--   { _typeSig_forall :: Maybe (NonEmpty [T.Text])
--   , _typeSig_classesUsed :: Maybe (NonEmpty [T.Text])
--   , _typeSig_inputTypes :: Maybe (NonEmpty [T.Text])
--   , _typeSig_resultType :: T.Text
--   }

-- Where type sig is a singular line
-- class RenderTypeSig a where
--   renderTypeSig :: a -> TypeSig
-- instance RenderTypeSig TypeSig where
--   renderTypeSig = id 
-- instance RenderTypeSign TypeSig' where
--   renderTypeSig tSig' =
--     let
--       forall =
--         maybe
--         ""
--         (\t -> )
--         (_typeSig_forall tSig')

existsClass :: ClassInfo -> Script
existsClass clsInfo =
  let
    className = _clsInfo_className clsInfo
    
    applyF :: (T.Text, TypeSig) {-function heads-} -> Script
    applyF (name, TypeSig typeSig) = Script $ T.pack [istr|
ace_requiredClass_Class_#{className} :: #{typeSig}
ace_requiredClass_Class_#{className} = #{name}
|]
  in
    mconcat $ applyF <$> _clsInfo_functionHeads clsInfo
    
existsDataConstructor :: DataConstructorName -> Script
existsDataConstructor dName = existsFunctionName' dName

existsDataConstructors :: [DataConstructorName] -> Script
existsDataConstructors dNames = existsFunctionNames dNames

existsFunctionNames :: [FunctionName] -> Script
existsFunctionNames [] = Script ""
existsFunctionNames (dName:xs) =
  existsFunctionName' dName <> existsFunctionNames xs

-- -- -- Take beginner friendly templates and make them something we can export
-- userPureFunctionToModule :: ModuleName -> Imports -> Function_ -> Script
-- userPureFunctionToModule (ModuleName mdlName) (Imports imports) func =
--   Script $
--   "module " <> mdlName <> " where "
--   <> T.unlines imports
--   <> T.pack
--   [istr|
-- #{script}

-- main = 
-- |]

-- -- -- Take beginner friendly templates and make them something we can export
-- userImpureFunctionToModule :: ModuleName -> Imports -> Function_ -> Script
-- userImpureFunctionToModule (ModuleName mdlName) (Imports imports) func =
--   Script $
--   "module " <> mdlName <> " where "
--   <> T.unlines imports
--   <> T.pack
--   [istr|
-- #{script}
-- |]
    

-- mk :: FunctionName -> FunctionName
-- f = myFunctionName

-- OR
-- mk :: FunctionalCase -> IO ()

newtype Tests = Tests T.Text

