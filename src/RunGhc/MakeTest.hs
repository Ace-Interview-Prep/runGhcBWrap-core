{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.MakeTest where

import RunGhc.Locate
import RunGhc.LocatedModule
import RunGhc.SystemModule
import RunGhc.Executable
import RunGhc.MakeExe
import RunGhc.UserInput

import GHC.Generics
import Text.IStr
import Control.Monad (replicateM)
import Data.Typeable
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Default
import Data.Bifunctor
import Data.Text (Text, pack)
import System.Random (randomRIO)
import System.Exit
import Data.Char (toUpper)

-- new idea : standardized Main.hs that runs 2 sets of symbols

-- Could also do these script builders by Arity


--x = compareFunc "f" mkF

--mkExample fname =

-- For modeling user-input
data SourceCode = SourceCode
  { _sourceCode_code :: T.Text
  , _sourceCode_target :: FunctionName
  } deriving Generic
instance FromJSON SourceCode
instance ToJSON SourceCode

-- The scope of our computation the user can see
data CodeChallengeResult = CodeChallengeResult
  { _ccrRunGhc_exitCode :: ExitCode
  , _ccrRunGhc_stderr :: [String]
  , _ccrRunGhc_tests :: [Bool]
  } deriving Generic
instance FromJSON CodeChallengeResult
instance ToJSON CodeChallengeResult

class Reduce a where
  reduce :: a -> [Bool]

instance Reduce CodeChallengeResult where
  reduce = _ccrRunGhc_tests

instance Reduce TestSolutionResult where
  reduce tsol = fmap _success $ _testSolRunGhc_tests tsol

-- The scope of our computation the user can see
data TestSolutionResult = TestSolutionResult
  { _testSolRunGhc_exitCode :: ExitCode
  , _testSolRunGhc_stderr :: [String]
  , _testSolRunGhc_tests :: [TryCodeResult T.Text T.Text]
  } deriving Generic
instance FromJSON TestSolutionResult
instance ToJSON TestSolutionResult



-- A neat hack to pass back the result like a typed-ffi
-- This is because we can import this in both the executable 
-- passed to runghcand also import in the program which
-- calls runghc. 
data TryCodeResult a b = TryCodeResult
  { _input :: a -- TODO: better typing
  , _output :: b -- TODO: better typing
  , _expectedOutput :: b
  , _success :: Bool
  } deriving Generic

instance Functor (TryCodeResult a) where
  fmap f tryCodeResult = TryCodeResult
    (_input tryCodeResult)
    (f $ _output tryCodeResult)
    (f $ _expectedOutput tryCodeResult)
    (_success tryCodeResult)

instance Bifunctor TryCodeResult where
  bimap f g (TryCodeResult inp out expected success) = 
    TryCodeResult (f inp) (g out) (g expected) success

instance (ToJSON a, ToJSON b) => ToJSON (TryCodeResult a b)
instance (FromJSON a, FromJSON b) => FromJSON (TryCodeResult a b)

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

-- A script definition that defers picking names until later, this is why it contains a function
newtype TypedNoNameScript input output = TypedNoNameScript
  { getTypedNoNameScript
    :: ModuleName {-random name-}
    -> FunctionName {-random name-}
    -> VarName{-random name-}
    -> TypeInfo (T input) (T output)
    -> Script
    -- ^ Should we instead make this Locatable a => a ?
  }

typedNoNameScriptExample :: TypedNoNameScript (Int, Bool) Int
typedNoNameScriptExample = TypedNoNameScript $ \(ModuleName moduleName) fname varName (TypeInfo input output) ->
  let (int, bool) = arity2Str input
      intOut = arity1Str output 
  in Script $ T.pack [istr|
module #{moduleName} where

#{varName} :: IO [(#{int}, #{bool})]
#{varName} = pure $ zip [1..10] (even <$> [11..20])

#{fname} :: #{int} -> #{bool} -> #{intOut}
#{fname} x y = if y then x * 2 else x * 5
|]


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
compareFuncExample = comparePureArity1 nnScriptExample $
  SourceCode
  { _sourceCode_code = exampleUserScript
  , _sourceCode_target = "userFunc"
  }

comparePureArityT1 :: TypedNoNameScript a b -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT1 = compareFuncTyped liftPureT1

comparePureArityT2 :: TypedNoNameScript (a, b) c -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT2 = compareFuncTyped liftPureT2

comparePureArityT3 :: TypedNoNameScript (a, b, c) d -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT3 = compareFuncTyped liftPureT3

comparePureArityT4 :: TypedNoNameScript (a, b, c, d) e -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT4 = compareFuncTyped liftPureT4

comparePureArityT5 :: TypedNoNameScript (a, b, c, d, e) f -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT5 = compareFuncTyped liftPureT5

comparePureArityT6 :: TypedNoNameScript (a, b, c, d, e, f) g -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT6 = compareFuncTyped liftPureT6

comparePureArityT7 :: TypedNoNameScript (a, b, c, d, e, f, g) h -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT7 = compareFuncTyped liftPureT7

comparePureArityT8 :: TypedNoNameScript (a, b, c, d, e, f, g, h) i -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT8 = compareFuncTyped liftPureT8

comparePureArityT9 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i) j -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT9 = compareFuncTyped liftPureT9

comparePureArityT10 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j) k -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT10 = compareFuncTyped liftPureT10

comparePureArityT11 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k) l -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT11 = compareFuncTyped liftPureT11

comparePureArityT12 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l) m -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT12 = compareFuncTyped liftPureT12

comparePureArityT13 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m) n -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT13 = compareFuncTyped liftPureT13

comparePureArityT14 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n) o -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT14 = compareFuncTyped liftPureT14

comparePureArityT15 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) p -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT15 = compareFuncTyped liftPureT15

comparePureArityT16 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) q -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT16 = compareFuncTyped liftPureT16

comparePureArityT17 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) r -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT17 = compareFuncTyped liftPureT17

comparePureArityT18 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) s -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT18 = compareFuncTyped liftPureT18

comparePureArityT19 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) t -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT19 = compareFuncTyped liftPureT19

comparePureArityT20 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) u -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT20 = compareFuncTyped liftPureT20

liftPureT1 :: FunctionName -> FnT a b
liftPureT1 = liftFnT . mkFT1

liftPureT2 :: FunctionName -> FnT (a,b) c
liftPureT2 = liftFnT . mkFT2

liftPureT3 :: FunctionName -> FnT (a,b,c) d
liftPureT3 = liftFnT . mkFT3

liftPureT4 :: FunctionName -> FnT (a,b,c,d) e
liftPureT4 = liftFnT . mkFT4

liftPureT5 :: FunctionName -> FnT (a,b,c,d,e) f
liftPureT5 = liftFnT . mkFT5

liftPureT6 :: FunctionName -> FnT (a,b,c,d,e,f) g
liftPureT6 = liftFnT . mkFT6

liftPureT7 :: FunctionName -> FnT (a,b,c,d,e,f,g) h
liftPureT7 = liftFnT . mkFT7

liftPureT8 :: FunctionName -> FnT (a,b,c,d,e,f,g,h) i
liftPureT8 = liftFnT . mkFT8

liftPureT9 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i) j
liftPureT9 = liftFnT . mkFT9

liftPureT10 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j) k
liftPureT10 = liftFnT . mkFT10

liftPureT11 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k) l
liftPureT11 = liftFnT . mkFT11

liftPureT12 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l) m
liftPureT12 = liftFnT . mkFT12

liftPureT13 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m) n
liftPureT13 = liftFnT . mkFT13

liftPureT14 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n) o
liftPureT14 = liftFnT . mkFT14

liftPureT15 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) p
liftPureT15 = liftFnT . mkFT15

liftPureT16 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) q
liftPureT16 = liftFnT . mkFT16

liftPureT17 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) r
liftPureT17 = liftFnT . mkFT17

liftPureT18 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) s
liftPureT18 = liftFnT . mkFT18

liftPureT19 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) t
liftPureT19 = liftFnT . mkFT19

liftPureT20 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) u
liftPureT20 = liftFnT . mkFT20

mkFT1 :: FunctionName -> FnT a b
mkFT1 fname = FnT . pack $ [istr| \a   -> #{fname} a|] 

mkFT2 :: FunctionName -> FnT (a,b) c
mkFT2 fname = FnT . pack $ [istr| \(a, b) -> #{fname} a b|]

mkFT3 :: FunctionName -> FnT (a,b,c) d
mkFT3 fname = FnT . pack $ [istr| \(a, b, c) -> #{fname} a b c|]

mkFT4 :: FunctionName -> FnT (a,b,c,d) e
mkFT4 fname = FnT . pack $ [istr| \(a, b, c, d) -> #{fname} a b c d|]

mkFT5 :: FunctionName -> FnT (a,b,c,d,e) f
mkFT5 fname = FnT . pack $ [istr| \(a, b, c, d, e) -> #{fname} a b c d e|]

mkFT6 :: FunctionName -> FnT (a,b,c,d,e,f) g
mkFT6 fname = FnT . pack $ [istr| \(a, b, c, d, e, f) -> #{fname} a b c d e f|]

mkFT7 :: FunctionName -> FnT (a,b,c,d,e,f,g) h
mkFT7 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g) -> #{fname} a b c d e f g|]

mkFT8 :: FunctionName -> FnT (a,b,c,d,e,f,g,h) i
mkFT8 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h) -> #{fname} a b c d e f g h|]

mkFT9 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i) j
mkFT9 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i) -> #{fname} a b c d e f g h i|]

mkFT10 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j) k
mkFT10 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j) -> #{fname} a b c d e f g h i j|]

mkFT11 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k) l
mkFT11 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k) -> #{fname} a b c d e f g h i j k|]

mkFT12 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l) m
mkFT12 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l) -> #{fname} a b c d e f g h i j k l|]

mkFT13 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m) n
mkFT13 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m) -> #{fname} a b c d e f g h i j k l m|]

mkFT14 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n) o
mkFT14 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> #{fname} a b c d e f g h i j k l m n|]

mkFT15 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) p
mkFT15 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> #{fname} a b c d e f g h i j k l m n o|]

mkFT16 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) q
mkFT16 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> #{fname} a b c d e f g h i j k l m n o p|]

mkFT17 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) r
mkFT17 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> #{fname} a b c d e f g h i j k l m n o p q|]

mkFT18 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) s
mkFT18 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> #{fname} a b c d e f g h i j k l m n o p q r|]

mkFT19 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) t
mkFT19 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> #{fname} a b c d e f g h i j k l m n o p q r s|]

mkFT20 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) u
mkFT20 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> #{fname} a b c d e f g h i j k l m n o p q r s t|]



comparePureArity1 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity1 = compareFunc liftPure1

comparePureArity2 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity2 = compareFunc liftPure2

comparePureArity3 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity3 = compareFunc liftPure3

comparePureArity4 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity4 = compareFunc liftPure4

comparePureArity5 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity5 = compareFunc liftPure5

comparePureArity6 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity6 = compareFunc liftPure6

comparePureArity7 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity7 = compareFunc liftPure7

comparePureArity8 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity8 = compareFunc liftPure8

comparePureArity9 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity9 = compareFunc liftPure9

comparePureArity10 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity10 = compareFunc liftPure10

comparePureArity11 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity11 = compareFunc liftPure11

comparePureArity12 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity12 = compareFunc liftPure12

comparePureArity13 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity13 = compareFunc liftPure13

comparePureArity14 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity14 = compareFunc liftPure14

comparePureArity15 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity15 = compareFunc liftPure15

comparePureArity16 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity16 = compareFunc liftPure16

comparePureArity17 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity17 = compareFunc liftPure17

comparePureArity18 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity18 = compareFunc liftPure18

comparePureArity19 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity19 = compareFunc liftPure19

comparePureArity20 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
comparePureArity20 = compareFunc liftPure20

compareMonadicArity1 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity1 = compareFunc mkF1

compareMonadicArity2 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity2 = compareFunc mkF2

compareMonadicArity3 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity3 = compareFunc mkF3

compareMonadicArity4 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity4 = compareFunc mkF4

compareMonadicArity5 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity5 = compareFunc mkF5

compareMonadicArity6 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity6 = compareFunc mkF6

compareMonadicArity7 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity7 = compareFunc mkF7

compareMonadicArity8 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity8 = compareFunc mkF8

compareMonadicArity9 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity9 = compareFunc mkF9

compareMonadicArity10 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity10 = compareFunc mkF10

compareMonadicArity11 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity11 = compareFunc mkF11

compareMonadicArity12 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity12 = compareFunc mkF12

compareMonadicArity13 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity13 = compareFunc mkF13

compareMonadicArity14 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity14 = compareFunc mkF14

compareMonadicArity15 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity15 = compareFunc mkF15

compareMonadicArity16 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity16 = compareFunc mkF16

compareMonadicArity17 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity17 = compareFunc mkF17

compareMonadicArity18 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity18 = compareFunc mkF18

compareMonadicArity19 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity19 = compareFunc mkF19

compareMonadicArity20 :: NoNameScript -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArity20 = compareFunc mkF20

compareFuncTyped
  :: forall a b.
     (FunctionName -> FnT a b)
  -- ^ Essentially, this means pick the arity of the problem
  -> TypedNoNameScript a b
  -- ^ The provided solution and input data as a haskell module
  -> SourceCode
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFuncTyped mkFn mkScript sourceCode = do -- (fnameUser, userScript) = do
  testVarName <- ((<>) "t") <$> randomAlphaNumCamelCaseName 5
  -- Must start with a Capital
  moduleName <- ((<>) "M") <$> randomAlphaNumCamelCaseName 5
  testFuncName <- ((<>) "f") <$> randomAlphaNumCamelCaseName 5
  
  let
    testLibScript :: Script
    testLibScript = (getTypedNoNameScript mkScript) (ModuleName moduleName) testFuncName testVarName (TypeInfo Proxy Proxy)
    locatedTestLib :: LocatedTestModule 
    locatedTestLib = LocatedTestModule $ locate [PathSegment moduleName] $ testLibScript

    settings_ = def
    userModule = mkUserModule' settings_ $ _sourceCode_code sourceCode
    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTypedTestScript
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, _sourceCode_target sourceCode)
      (localImport $ getLocatedTestModule locatedTestLib, testFuncName)
    locatedMainModule = LocatedTestModule $ locate [PathSegment "Main"] mainModule
    
  pure $ Right $ toCompareExe userModule locatedTestLib locatedMainModule


compareFunc
  :: (FunctionName -> Fn)
  -- ^ Essentially, this means pick the arity of the problem
  -> NoNameScript
  -- ^ The provided solution and input data as a haskell module
  -> SourceCode
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFunc mkFn mkScript sourceCode = do -- (fnameUser, userScript) = do
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
    userModule = mkUserModule' settings_ $ _sourceCode_code sourceCode
    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTestScript
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, _sourceCode_target sourceCode)
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

-- Perhaps we can derive this func we pass as an arg?
genMakeMainComparativeTypedTestScript
  :: (FunctionName -> FnT a b)
  -> VarName
  -> (Import, FunctionName) -- UserModuleName -- In theory, we could directly get this from the Import type
  -> (Import, FunctionName) -- TestModule     -- In theory, we could directly get this from the Import type
  -> TestScript
genMakeMainComparativeTypedTestScript mkF testValueName (userModule, fnameUser) (testModule, fnameCompare) = Script $ pack [istr|
module Main where

import Control.Monad (mapM)
#{showImportLine userModule}
#{showImportLine testModule}
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import RunGhc.MakeTest


--userF, solutionF -- should i use the proxies to assert the types here?
userF = #{getFnT . mkF $ importName (getImportName userModule) <> "." <> fnameUser}
solutionF = #{getFnT . mkF $ importName (getImportName testModule) <> "." <> fnameCompare}

main :: IO ()
main = do
  inputs <- #{importName $ getImportName testModule}.#{testValueName} -- inputs :: (ToJSON a, FromJSON a) => a 
  valsUser <- mapM userF inputs
  valsOurSolution <- mapM solutionF inputs
  let x = zipWith3 (\outUser expec_ inp -> TryCodeResult inp outUser expec_ (outUser == expec_))  valsUser valsOurSolution inputs
  LBS.putStrLn $ Aeson.encode x 
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

data FnT a b = FnT { getFnT :: T.Text }

newtype Fn = Fn { getFn :: T.Text }
chainFn :: Fn -> Fn -> Fn
chainFn f1 f2 = Fn $ getFn (wrap f1) <> " . " <> getFn (wrap f2)

chainFnT :: FnT a b -> FnT b c -> FnT a c
chainFnT f1 f2 = FnT $ getFnT (wrapT f1) <> " . " <> getFnT (wrapT f2)

wrapT :: FnT a b -> FnT a b
wrapT f = FnT ("(" <> getFnT f <> ")") 

wrap :: Fn -> Fn
wrap f = Fn $ "(" <> getFn f <> ")" 

-- | TODO: this should really be (m b) or even (m a) but I have no idea how to do this just yet
------- :: Fn  a b -> FnT a (m a)
liftFnT :: FnT a b -> FnT a b
liftFnT f = pure_ `chainFnT` f
  where pure_ = FnT "pure"

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


-- A set of functions
data TypeInfo a b = TypeInfo
  { _typesIn :: a
  , _typeOut :: b
  }
type T a = Proxy a
type_ :: T a
type_ = Proxy

arity1 :: T a -> T a
arity1 = id

arity2 :: T (a, b) -> (T a, T b)
arity2 _ = (type_, type_)

arity3 :: T (a, b, c) -> (T a, T b, T c)
arity3 _ = (type_, type_, type_)

arity4 :: T (a, b, c, d) -> (T a, T b, T c, T d)
arity4 _ = (type_, type_, type_, type_)

arity5 :: T (a, b, c, d, e) -> (T a, T b, T c, T d, T e)
arity5 _ = (type_, type_, type_, type_, type_)

arity6 :: T (a, b, c, d, e, f) -> (T a, T b, T c, T d, T e, T f)
arity6 _ = (type_, type_, type_, type_, type_, type_)

arity7 :: T (a, b, c, d, e, f, g) -> (T a, T b, T c, T d, T e, T f, T g)
arity7 _ = (type_, type_, type_, type_, type_, type_, type_)

arity8 :: T (a, b, c, d, e, f, g, h) -> (T a, T b, T c, T d, T e, T f, T g, T h)
arity8 _ = (type_, type_, type_, type_, type_, type_, type_, type_)

arity9 :: T (a, b, c, d, e, f, g, h, i) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i)
arity9 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity10 :: T (a, b, c, d, e, f, g, h, i, j) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j)
arity10 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity11 :: T (a, b, c, d, e, f, g, h, i, j, k) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k)
arity11 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity12 :: T (a, b, c, d, e, f, g, h, i, j, k, l) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l)
arity12 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity13 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m)
arity13 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity14 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n)
arity14 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity15 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o)
arity15 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity16 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p)
arity16 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity17 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q)
arity17 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity18 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r)
arity18 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity19 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r, T s)
arity19 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity20 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r, T s, T t)
arity20 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity1Str :: Typeable a => T a -> String
arity1Str = show . typeRep

arity2Str :: (Typeable a, Typeable b) => T (a, b) -> (String, String)
arity2Str x =
    let (a, b) = arity2 x
    in (show (typeRep a), show (typeRep b))

arity3Str :: (Typeable a, Typeable b, Typeable c) => T (a, b, c) -> (String, String, String)
arity3Str x =
    let (a, b, c) = arity3 x
    in (show (typeRep a), show (typeRep b), show (typeRep c))

arity4Str :: (Typeable a, Typeable b, Typeable c, Typeable d) => T (a, b, c, d) -> (String, String, String, String)
arity4Str x =
    let (a, b, c, d) = arity4 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d))

arity5Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => T (a, b, c, d, e) -> (String, String, String, String, String)
arity5Str x =
    let (a, b, c, d, e) = arity5 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e))

arity6Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => T (a, b, c, d, e, f) -> (String, String, String, String, String, String)
arity6Str x =
    let (a, b, c, d, e, f) = arity6 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f))

arity7Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g) => T (a, b, c, d, e, f, g) -> (String, String, String, String, String, String, String)
arity7Str x =
    let (a, b, c, d, e, f, g) = arity7 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g))

arity8Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h) => T (a, b, c, d, e, f, g, h) -> (String, String, String, String, String, String, String, String)
arity8Str x =
    let (a, b, c, d, e, f, g, h) = arity8 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h))

arity9Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i) => T (a, b, c, d, e, f, g, h, i) -> (String, String, String, String, String, String, String, String, String)
arity9Str x =
    let (a, b, c, d, e, f, g, h, i') = arity9 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'))

arity10Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j) => T (a, b, c, d, e, f, g, h, i, j) -> (String, String, String, String, String, String, String, String, String, String)
arity10Str x =
    let (a, b, c, d, e, f, g, h, i', j) = arity10 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j))

arity11Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k) => T (a, b, c, d, e, f, g, h, i, j, k) -> (String, String, String, String, String, String, String, String, String, String, String)
arity11Str x =
    let (a, b, c, d, e, f, g, h, i', j, k) = arity11 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k))

arity12Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l) => T (a, b, c, d, e, f, g, h, i, j, k, l) -> (String, String, String, String, String, String, String, String, String, String, String, String)
arity12Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l) = arity12 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l))

arity13Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m) => T (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (String, String, String, String, String, String, String, String, String, String, String, String, String)
arity13Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m) = arity13 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m))

arity14Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity14Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n) = arity14 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n))

arity15Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity15Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o) = arity15 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o))

arity16Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity16Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p) = arity16 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p))

arity17Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity17Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q) = arity17 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q))

arity18Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity18Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r) = arity18 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r))

arity19Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r, Typeable s) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity19Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r, s) = arity19 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r), show (typeRep s))

arity20Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r, Typeable s, Typeable t) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity20Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r, s, t) = arity20 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r), show (typeRep s), show (typeRep t))

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

  
instance ToJSON ExitCode
instance FromJSON ExitCode
