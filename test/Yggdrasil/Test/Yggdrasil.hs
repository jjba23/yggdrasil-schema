import Optics hiding (uncons)
import Relude
import System.Directory
import System.Random
import Test.Hspec

yggdrasilSpec = describe "yggdrasil" $ do
  it "hspec works" $ do
    True `shouldBe` True
