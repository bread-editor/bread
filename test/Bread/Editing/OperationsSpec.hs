module Bread.Editing.OperationsSpec (main, spec) where

import Test.Hspec
import Data.Sequence as S
import Bread.Editing.Operations as Ops

main = hspec spec

spec = do
  describe "insert" $ do
    it "adds a character at the proper position" $ do
      Ops.insert 'f' 5 (S.fromList "abcdegh") `shouldBe` (S.fromList "abcdefgh")

  describe "insertString" $ do
    it "inserts a string of characters at the proper position" $ do
      Ops.insertString "Hello" 0 (S.fromList ", world!") `shouldBe` (S.fromList "Hello, world!")

  describe "delete" $ do
    it "removes a character at the proper position" $ do
      Ops.delete 8 (S.fromList "abcdefghiijk") `shouldBe` (S.fromList "abcdefghijk")

  describe "deleteRange" $ do
    it "removes a range of characters properly" $ do
      Ops.deleteRange (3, 6) (S.fromList "ThiTESTs is a cool message") `shouldBe` (S.fromList "This is a cool message")

  describe "copy" $ do
    it "copies a character properly" $ do
      Ops.copy 4 (S.fromList "abcdefg") `shouldBe` (S.fromList "e")

  describe "copyRange" $ do
    it "copies a range of characters properly" $ do
      Ops.copyRange (3, 5) (S.fromList "abcdefg") `shouldBe` (S.fromList "def")

  describe "cut" $ do
    it "cuts a character and returns the resultant buffer and character properly" $ do
      Ops.cut 5 (S.fromList "abcdefg") `shouldBe` (S.fromList "abcdeg", S.fromList "f")

  describe "cutRange" $ do
    it "cuts a range of characters and returns the resultant buffer and characters properly" $ do
      Ops.cutRange (3, 5) (S.fromList "abcdefg") `shouldBe` (S.fromList "abcg", S.fromList "def")
