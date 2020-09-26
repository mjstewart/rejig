module HelloSpec
  ( spec,
  )
where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "add" $ do
    it "should add" $
      property $ \x y ->
        add x y == add y x

-- describe "me"
-- it "hi" $ property $ \x y ->
-- add x y == add y x

add :: Int -> Int -> Int
add x y = x + y
