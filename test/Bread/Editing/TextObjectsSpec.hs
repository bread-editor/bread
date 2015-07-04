module Bread.Editing.TextObjectsSpec (main, spec) where

import Test.Hspec
import Bread.Editing.TextObjects
import Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec =  do
  describe "getChars" $ do
    it "gets chars between a range" $ do
      getChars (4, 7) (T.pack "Ayy lmao") `shouldBe` "lmao"

  describe "getCharSeq" $ do
    it "gets chars between a range" $ do
      getCharSeq (4, 7) (T.pack "Ayy lmao") `shouldBe` T.pack "lmao"
      
