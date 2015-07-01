module Bread.Core.FilesSpec (main, spec) where
import Test.Hspec
import qualified Bread.Core.Files as B
import qualified Data.Sequence as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Files" $ do
    it "loads a file correctly" $ do
      contents <- B.readFile "./test/testfile.txt"
      contents `shouldBe` S.fromList "This is a test file.\nIt contains test contents. With other things.\n"
  
