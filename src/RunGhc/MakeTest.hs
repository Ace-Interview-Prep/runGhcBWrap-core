module RunGhc.MakeTest where


data Generator
  = StaticConstant T.Text
  | Unfoldr  

newtype Filter = Filter T.Text
newtype Count = Count Int
makeTests :: Count -> Filter -> Generator -> Tests
makeTests (Count ct) (Filter filt) generator = Tests $ 
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

  
