module Bread.Data.FilesSpec (main, spec) where
import Test.Hspec
import Bread.Data.Files as B
import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Files" $ do
    it "loads a file correctly" $ do
      con <- B.readFile "./test/testfile.txt"
      B.contents con `shouldBe` "This is a test file.\nIt contains test contents. With other things.\n"
      B.name con `shouldBe` "testfile.txt"
  
