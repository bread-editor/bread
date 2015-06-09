module Bread.Core.Editing.OperationsSpec (main, spec) where

import Test.Hspec
import Data.Sequence as S
import Bread.Core.Editing.Operations as Ops

main = hspec spec

spec = do
  describe "insert" $ do
    it "adds a character at the proper position" $ do
      Ops.insert 'f' 5 (S.fromList "abcdegh") `shouldBe` (S.fromList "abcdefgh")

  describe "delete" $ do
    it "removes a character at the proper position" $ do
      Ops.delete 8 (S.fromList "abcdefghiijk") `shouldBe` (S.fromList "abcdefghijk")
      
