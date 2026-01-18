{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module RunGhc.MakeTest where

import RunGhc.Locate
import RunGhc.LocatedModule
import RunGhc.SystemModule
import RunGhc.Executable
import RunGhc.MakeExe
import RunGhc.UserInput

import Text.IStr
import Control.Monad (replicateM)
import Data.Typeable
import Data.Maybe
import qualified Data.Text as T
import Data.Default
import Data.Text (Text, pack)
import System.Random (randomRIO)
import Data.Char (toUpper)

-- new idea : standardized Main.hs that runs 2 sets of symbols

-- Could also do these script builders by Arity


--x = compareFunc "f" mkF

--mkExample fname =


toCompareExe
  :: LocatedUserModule -- User
  -> LocatedTestModule -- System: Compare Against
  -> LocatedTestModule -- Run Compare
  -> Executable
toCompareExe (LocatedUserModule locUser) (LocatedTestModule locTestLib) (LocatedTestModule locTestMain) =
  Executable
  { _main = locTestMain
  , _library = [locUser, locTestLib]
  }

-- There's no core reason we need to have the names be the same
--
-- Also the only name we ever need to actually know (prior to runtime generation of the exe)
-- is the compilation target of the user's module
--
-- There is truly no issue with randomly generating names. This will also help us to
-- enforce that the user just cannot ever get the references  #{moduleName}.#{testFunction} or #{moduleName}.#{testData}
-- and in fact, we don't even know! only the compareFunc will for a sec!

-- A script definition that defers picking names until later, this is why it contains a function
newtype NoNameScript = NoNameScript
  { getNoNameScript
    :: ModuleName {-random name-}
    -> FunctionName {-random name-}
    -> VarName{-random name-}
    -> Script
    -- ^ Should we instead make this Locatable a => a ?
  }

type VarName = T.Text -- the reference to the Test
nnScriptExample :: NoNameScript
nnScriptExample = NoNameScript $ \(ModuleName moduleName) fname varName -> Script $ T.pack [istr|
module #{moduleName} where

#{varName} :: IO [(Int, Int)]
#{varName} = pure $ zip [1..10] [11..20]

#{fname} :: Int -> Int -> Int
#{fname} x y = x + y
|]
  
exampleUserScript :: T.Text
exampleUserScript = T.pack [istr|
module UserModule where
userFunc :: Int -> Int -> Int
userFunc x y = x + y
|]

compareFuncExample :: IO (Either T.Text Executable)
compareFuncExample = comparePureArity1 nnScriptExample ("userFunc", exampleUserScript)

comparePureArity1 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity1 = compareFunc liftPure1

comparePureArity2 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity2 = compareFunc liftPure2

comparePureArity3 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity3 = compareFunc liftPure3

comparePureArity4 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity4 = compareFunc liftPure4

comparePureArity5 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity5 = compareFunc liftPure5

comparePureArity6 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity6 = compareFunc liftPure6

comparePureArity7 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity7 = compareFunc liftPure7

comparePureArity8 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity8 = compareFunc liftPure8

comparePureArity9 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity9 = compareFunc liftPure9

comparePureArity10 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity10 = compareFunc liftPure10

comparePureArity11 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity11 = compareFunc liftPure11

comparePureArity12 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity12 = compareFunc liftPure12

comparePureArity13 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity13 = compareFunc liftPure13

comparePureArity14 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity14 = compareFunc liftPure14

comparePureArity15 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity15 = compareFunc liftPure15

comparePureArity16 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity16 = compareFunc liftPure16

comparePureArity17 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity17 = compareFunc liftPure17

comparePureArity18 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity18 = compareFunc liftPure18

comparePureArity19 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity19 = compareFunc liftPure19

comparePureArity20 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
comparePureArity20 = compareFunc liftPure20

compareMonadicArity1 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity1 = compareFunc mkF1

compareMonadicArity2 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity2 = compareFunc mkF2

compareMonadicArity3 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity3 = compareFunc mkF3

compareMonadicArity4 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity4 = compareFunc mkF4

compareMonadicArity5 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity5 = compareFunc mkF5

compareMonadicArity6 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity6 = compareFunc mkF6

compareMonadicArity7 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity7 = compareFunc mkF7

compareMonadicArity8 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity8 = compareFunc mkF8

compareMonadicArity9 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity9 = compareFunc mkF9

compareMonadicArity10 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity10 = compareFunc mkF10

compareMonadicArity11 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity11 = compareFunc mkF11

compareMonadicArity12 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity12 = compareFunc mkF12

compareMonadicArity13 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity13 = compareFunc mkF13

compareMonadicArity14 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity14 = compareFunc mkF14

compareMonadicArity15 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity15 = compareFunc mkF15

compareMonadicArity16 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity16 = compareFunc mkF16

compareMonadicArity17 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity17 = compareFunc mkF17

compareMonadicArity18 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity18 = compareFunc mkF18

compareMonadicArity19 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity19 = compareFunc mkF19

compareMonadicArity20 :: NoNameScript -> (FunctionName, T.Text) -> IO (Either T.Text Executable)
compareMonadicArity20 = compareFunc mkF20



compareFunc
  :: (FunctionName -> Fn)
  -- ^ Essentially, this means pick the arity of the problem
  -> NoNameScript
  -- ^ The provided solution and input data as a haskell module
  -> (FunctionName, T.Text)
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFunc mkFn mkScript (fnameUser, userScript) = do
  testVarName <- ((<>) "t") <$> randomAlphaNumCamelCaseName 5
  -- Must start with a Capital
  moduleName <- ((<>) "M") <$> randomAlphaNumCamelCaseName 5
  testFuncName <- ((<>) "f") <$> randomAlphaNumCamelCaseName 5
  
  let
    testLibScript :: Script
    testLibScript = (getNoNameScript mkScript) (ModuleName moduleName) testFuncName testVarName
    locatedTestLib :: LocatedTestModule 
    locatedTestLib = LocatedTestModule $ locate [PathSegment moduleName] $ testLibScript

    settings_ = def
    userModule = mkUserModule' settings_ userScript 

    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTestScript
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, fnameUser)
      (localImport $ getLocatedTestModule locatedTestLib, testFuncName)
    locatedMainModule = LocatedTestModule $ locate [PathSegment "Main"] mainModule
    
  pure $ Right $ toCompareExe userModule locatedTestLib locatedMainModule

randomAlphaNumCamelCaseName :: Int -> IO T.Text
randomAlphaNumCamelCaseName numWords = do
    words <- replicateM numWords randomWord
    pure $ T.concat $ zipWith capitalize [0..] words
  where
    alphaNum = ['a'..'z'] ++ ['0'..'9']
    randomWord :: IO T.Text
    randomWord = do
        len <- randomRIO (3, 8)
        chars <- replicateM len $ do
            idx <- randomRIO (0, length alphaNum - 1)
            pure $ alphaNum !! idx
        pure $ T.pack chars
    capitalize :: Int -> T.Text -> T.Text
    capitalize 0 w = w  -- first word stays lowercase
    capitalize _ w = case T.uncons w of
        Nothing -> w
        Just (c, rest) -> T.cons (toUpper c) rest
   
type TestScript = Script
-- Perhaps we can derive this func we pass as an arg?
genMakeMainComparativeTestScript
  :: (FunctionName -> Fn)
  -> VarName
  -> (Import, FunctionName) -- UserModuleName -- In theory, we could directly get this from the Import type
  -> (Import, FunctionName) -- TestModule     -- In theory, we could directly get this from the Import type
  -> TestScript
genMakeMainComparativeTestScript mkF testValueName (userModule, fnameUser) (testModule, fnameCompare) = Script $ pack [istr|
module Main where

import Control.Monad (mapM)
#{showImportLine userModule}
#{showImportLine testModule}

--userF, solutionF 
userF = #{getFn . mkF $ importName (getImportName userModule) <> "." <> fnameUser}
solutionF = #{getFn . mkF $ importName (getImportName testModule) <> "." <> fnameCompare}

main :: IO ()
main = do
  inputs <- #{importName $ getImportName testModule}.#{testValueName} -- inputs :: (ToJSON a, FromJSON a) => a 
  valsUser <- mapM userF inputs
  valsOurSolution <- mapM solutionF inputs
  print $ zipWith (==) valsUser valsOurSolution
|]

newtype ImportName = ImportName { importName :: T.Text }
getImportName :: Import -> ImportName
getImportName imp = ImportName $ fromMaybe (combine $ _import_pathSeg imp) (_import_qualifiedName imp)
  where
    combine = T.intercalate "." . fmap getPathSegment
-- get either qualified name or just the module name as path
-- keeping in mind that everything is technically a qualified import
-- where if we havent overrided the name it is just the Module's name itself
-- eg. import Data.Text ; f = Data.Text.pack

newtype TypedExpression a
  = TypedExpression T.Text
  
newtype Expression = Expression { getExpression :: T.Text }
-- To Enable a TypedExpression
  --where
    -- In theory, we could do something like this to typecheck a singular expression, before we include it in the script gen
    --mkExpression :: (a -> IO b) -> IO b
    --mkExpression f = $([e| 1 + 1 + g|] )

newtype TypedName a = TypedName { getTypedReference :: T.Text }

-- In theory we could do:
-- apply :: Function (a ': xs) -> TypedName a -> Function xs
--
-- and TypedName is gen'd by:
-- writeHaskellTopLevelVar :: VarName -> Expression -> TypedName 

newtype Fn = Fn { getFn :: T.Text }
chainFn :: Fn -> Fn -> Fn
chainFn f1 f2 = Fn $ getFn (wrap f1) <> " . " <> getFn (wrap f2)

wrap :: Fn -> Fn
wrap f = Fn $ "(" <> getFn f <> ")" 

liftFn :: Fn -> Fn
liftFn f = pure_ `chainFn` f
  where pure_ = Fn "pure"

-- This is the `id` function                    
mkF1 :: FunctionName -> Fn
mkF1 fname = Fn . pack $ [istr| \a -> #{fname} a|] 

mkF2 :: FunctionName -> Fn
mkF2 fname = Fn . pack $ [istr| \(a, b) -> #{fname} a b|]

mkF3 :: FunctionName -> Fn
mkF3 fname = Fn . pack $ [istr| \(a, b, c) -> #{fname} a b c|]

mkF4 :: FunctionName -> Fn
mkF4 fname = Fn . pack $ [istr| \(a, b, c, d) -> #{fname} a b c d|]

mkF5 :: FunctionName -> Fn
mkF5 fname = Fn . pack $ [istr| \(a, b, c, d, e) -> #{fname} a b c d e|]

mkF6 :: FunctionName -> Fn
mkF6 fname = Fn . pack $ [istr| \(a, b, c, d, e, f) -> #{fname} a b c d e f|]

mkF7 :: FunctionName -> Fn
mkF7 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g) -> #{fname} a b c d e f g|]

mkF8 :: FunctionName -> Fn
mkF8 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h) -> #{fname} a b c d e f g h|]

mkF9 :: FunctionName -> Fn
mkF9 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i) -> #{fname} a b c d e f g h i|]

mkF10 :: FunctionName -> Fn
mkF10 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j) -> #{fname} a b c d e f g h i j|]

mkF11 :: FunctionName -> Fn
mkF11 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k) -> #{fname} a b c d e f g h i j k|]

mkF12 :: FunctionName -> Fn
mkF12 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l) -> #{fname} a b c d e f g h i j k l|]

mkF13 :: FunctionName -> Fn
mkF13 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m) -> #{fname} a b c d e f g h i j k l m|]

mkF14 :: FunctionName -> Fn
mkF14 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> #{fname} a b c d e f g h i j k l m n|]

mkF15 :: FunctionName -> Fn
mkF15 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> #{fname} a b c d e f g h i j k l m n o|]

mkF16 :: FunctionName -> Fn
mkF16 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> #{fname} a b c d e f g h i j k l m n o p|]

mkF17 :: FunctionName -> Fn
mkF17 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> #{fname} a b c d e f g h i j k l m n o p q|]

mkF18 :: FunctionName -> Fn
mkF18 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> #{fname} a b c d e f g h i j k l m n o p q r|]

mkF19 :: FunctionName -> Fn
mkF19 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> #{fname} a b c d e f g h i j k l m n o p q r s|]

mkF20 :: FunctionName -> Fn
mkF20 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> #{fname} a b c d e f g h i j k l m n o p q r s t|]

liftPure1 :: FunctionName -> Fn
liftPure1 = liftFn . mkF1

liftPure2 :: FunctionName -> Fn
liftPure2 = liftFn . mkF2

liftPure3 :: FunctionName -> Fn
liftPure3 = liftFn . mkF3

liftPure4 :: FunctionName -> Fn
liftPure4 = liftFn . mkF4

liftPure5 :: FunctionName -> Fn
liftPure5 = liftFn . mkF5

liftPure6 :: FunctionName -> Fn
liftPure6 = liftFn . mkF6

liftPure7 :: FunctionName -> Fn
liftPure7 = liftFn . mkF7

liftPure8 :: FunctionName -> Fn
liftPure8 = liftFn . mkF8

liftPure9 :: FunctionName -> Fn
liftPure9 = liftFn . mkF9

liftPure10 :: FunctionName -> Fn
liftPure10 = liftFn . mkF10

liftPure11 :: FunctionName -> Fn
liftPure11 = liftFn . mkF11

liftPure12 :: FunctionName -> Fn
liftPure12 = liftFn . mkF12

liftPure13 :: FunctionName -> Fn
liftPure13 = liftFn . mkF13

liftPure14 :: FunctionName -> Fn
liftPure14 = liftFn . mkF14

liftPure15 :: FunctionName -> Fn
liftPure15 = liftFn . mkF15

liftPure16 :: FunctionName -> Fn
liftPure16 = liftFn . mkF16

liftPure17 :: FunctionName -> Fn
liftPure17 = liftFn . mkF17

liftPure18 :: FunctionName -> Fn
liftPure18 = liftFn . mkF18

liftPure19 :: FunctionName -> Fn
liftPure19 = liftFn . mkF19

liftPure20 :: FunctionName -> Fn
liftPure20 = liftFn . mkF20

-- runPure1 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure1 = genMakeMainComparativeTestScript liftPure1

-- runPure2 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure2 = genMakeMainComparativeTestScript liftPure2

-- runPure3 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure3 = genMakeMainComparativeTestScript liftPure3

-- runPure4 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure4 = genMakeMainComparativeTestScript liftPure4

-- runPure5 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure5 = genMakeMainComparativeTestScript liftPure5

-- runPure6 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure6 = genMakeMainComparativeTestScript liftPure6

-- runPure7 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure7 = genMakeMainComparativeTestScript liftPure7

-- runPure8 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure8 = genMakeMainComparativeTestScript liftPure8

-- runPure9 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure9 = genMakeMainComparativeTestScript liftPure9

-- runPure10 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure10 = genMakeMainComparativeTestScript liftPure10

-- runPure11 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure11 = genMakeMainComparativeTestScript liftPure11

-- runPure12 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure12 = genMakeMainComparativeTestScript liftPure12

-- runPure13 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure13 = genMakeMainComparativeTestScript liftPure13

-- runPure14 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure14 = genMakeMainComparativeTestScript liftPure14

-- runPure15 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure15 = genMakeMainComparativeTestScript liftPure15

-- runPure16 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure16 = genMakeMainComparativeTestScript liftPure16

-- runPure17 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure17 = genMakeMainComparativeTestScript liftPure17

-- runPure18 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure18 = genMakeMainComparativeTestScript liftPure18

-- runPure19 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure19 = genMakeMainComparativeTestScript liftPure19

-- runPure20 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure20 = genMakeMainComparativeTestScript liftPure20

-- runMonadic1 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic1 = genMakeMainComparativeTestScript mkF1

-- runMonadic2 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic2 = genMakeMainComparativeTestScript mkF2

-- runMonadic3 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic3 = genMakeMainComparativeTestScript mkF3

-- runMonadic4 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic4 = genMakeMainComparativeTestScript mkF4

-- runMonadic5 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic5 = genMakeMainComparativeTestScript mkF5

-- runMonadic6 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic6 = genMakeMainComparativeTestScript mkF6

-- runMonadic7 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic7 = genMakeMainComparativeTestScript mkF7

-- runMonadic8 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic8 = genMakeMainComparativeTestScript mkF8

-- runMonadic9 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic9 = genMakeMainComparativeTestScript mkF9

-- runMonadic10 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic10 = genMakeMainComparativeTestScript mkF10

-- runMonadic11 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic11 = genMakeMainComparativeTestScript mkF11

-- runMonadic12 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic12 = genMakeMainComparativeTestScript mkF12

-- runMonadic13 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic13 = genMakeMainComparativeTestScript mkF13

-- runMonadic14 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic14 = genMakeMainComparativeTestScript mkF14

-- runMonadic15 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic15 = genMakeMainComparativeTestScript mkF15

-- runMonadic16 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic16 = genMakeMainComparativeTestScript mkF16

-- runMonadic17 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic17 = genMakeMainComparativeTestScript mkF17

-- runMonadic18 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic18 = genMakeMainComparativeTestScript mkF18

-- runMonadic19 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic19 = genMakeMainComparativeTestScript mkF19

-- runMonadic20 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic20 = genMakeMainComparativeTestScript mkF20
  --where
    -- In theory, we could do something like this to typecheck a singular expression, before we include it in the script gen
    --mkExpression :: (a -> IO b) -> IO b
    --mkExpression f = $([e| 1 + 1 + g|] )



type EitherDesign = Either
script
  :: forall a
  .  Typeable a
  => LocatedUserModule -- The user's solution, to make 
  -> LocatedUserModule -- Our Correct Solution, to make an import
  -> EitherDesign [[a]] [a]
  -- We could choose the design of this being a list of type names OR just the tests themselves
  -- which would be all we need to get the type info
script = undefined   
-- script fname = [istr|
-- main = do
--   inputs <- makeTest #{test11Inputs} 

--   valsUser <- mapM User.#{fname} inputs
--   valsOurSolution <- mapM Solution.#{fname} inputs

--   print $ zipWith (==) valsUser valsOurSolution

-- |]


data Generator
  = StaticConstant T.Text
  | Unfoldr  

newtype Tests' = Tests' T.Text
newtype Filter = Filter T.Text
newtype Count = Count Int

makeTests :: Count -> Filter -> Generator -> Tests'
makeTests (Count ct) (Filter filt) generator = Tests' $ 
  "Prelude.take " <> (T.pack $ show ct)
  <> " $ "
  <> " filter "
  <> filt
  <> " "
  <> renderGenerator generator

renderGenerator :: Generator -> T.Text
renderGenerator = \case
  StaticConstant t -> t
  Unfoldr -> "[]"

  
