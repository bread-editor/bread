module Bread.Editing.OperationsSpec (main, spec) where

import Test.Hspec
import Data.Text as T
import Bread.Editing.Operations as Ops

main = hspec spec

spec = do
  describe "insert" $ do
    it "adds a character at the proper position" $ do
      Ops.insert 'f' 5 (pack "abcdegh") `shouldBe` (pack "abcdefgh")

  describe "insertString" $ do
    it "inserts a string of characters at the proper position" $ do
      Ops.insertString (pack "Hello") 0 (pack ", world!") `shouldBe` (pack "Hello, world!")

  describe "delete" $ do
    it "removes a character at the proper position" $ do
      Ops.delete 8 (pack "abcdefghiijk") `shouldBe` (pack "abcdefghijk")

  describe "deleteRange" $ do
    it "removes a range of characters properly" $ do
      Ops.deleteRange (3, 6) (pack "ThiTESTs is a cool message") `shouldBe` (pack "This is a cool message")

  describe "copy" $ do
    it "copies a character properly" $ do
      Ops.copy 4 (pack "abcdefg") `shouldBe` (pack "e")

  describe "copyRange" $ do
    it "copies a range of characters properly" $ do
      Ops.copyRange (3, 5) (pack "abcdefg") `shouldBe` (pack "def")

  describe "cut" $ do
    it "cuts a character and returns the resultant buffer and character properly" $ do
      Ops.cut 5 (pack "abcdefg") `shouldBe` (pack "abcdeg", pack "f")

  describe "cutRange" $ do
    it "cuts a range of characters and returns the resultant buffer and characters properly" $ do
      Ops.cutRange (3, 5) (pack "abcdefg") `shouldBe` (pack "abcg", pack "def")
