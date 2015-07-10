module Bread.Data.BufferSpec (main, spec) where
import Test.Hspec
import Bread.Data.Buffer as B
import Bread.Data.Bundle
import Data.Text
import Data.MessagePack as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Files" $ do
    it "loads a file correctly" $ do
      con <- B.readFile "./test/testfile.txt"
      B.contents con `shouldBe` "This is a test file.\nIt contains test contents. With other things.\n"
      B.name con `shouldBe` "testfile.txt"
      B.path con `shouldBe` "./test/testfile.txt"

    it "bundles a Buffer" $ do
      con <- B.readFile "./test/testfile.txt"
      bundle con `shouldBe` [TextVal "This is a test file.\nIt contains test contents. With other things.\n", TextVal "testfile.txt", TextVal "./test/testfile.txt"]
      
  
