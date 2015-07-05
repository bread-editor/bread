module Bread.Editing.OperationsSpec (main, spec) where

import Test.Hspec
import Data.Text as T
import Bread.Editing.Operations as Ops

main = hspec spec

spec = do
  describe "insert" $ do
    it "adds a character at the proper position" $ do
      Ops.insert 'f' 5 "abcdegh" `shouldBe` "abcdefgh"

  describe "insertString" $ do
    it "inserts a string of characters at the proper position" $ do
      Ops.insertString "Hello" 0 ", world!" `shouldBe` "Hello, world!"

  describe "delete" $ do
    it "removes a character at the proper position" $ do
      Ops.delete 8 "abcdefghiijk" `shouldBe` "abcdefghijk"

  describe "deleteRange" $ do
    it "removes a range of characters properly" $ do
      Ops.deleteRange (3, 6) "ThiTESTs is a cool message" `shouldBe` "This is a cool message"

  describe "copy" $ do
    it "copies a character properly" $ do
      Ops.copy 4 "abcdefg" `shouldBe` "e"

  describe "copyRange" $ do
    it "copies a range of characters properly" $ do
      Ops.copyRange (3, 5) "abcdefg" `shouldBe` "def"

  describe "cut" $ do
    it "cuts a character and returns the resultant buffer and character properly" $ do
      Ops.cut 5 "abcdefg" `shouldBe` ("abcdeg", "f")

  describe "cutRange" $ do
    it "cuts a range of characters and returns the resultant buffer and characters properly" $ do
      Ops.cutRange (3, 5) "abcdefg" `shouldBe` ("abcg", "def")
