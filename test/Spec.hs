import Test.Hspec
import Time
import Types
import GHC.Word (Word32)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "updateTime" $ do
            it "when should be a new frame"
                $ Time.updateTime (60, 1) (Time (60::Word32) (0::Word32) False)
                `shouldBe` Time (61::Word32) (61::Word32) True
            it "when should not be a new frame"
                $ Time.updateTime (60, 1) (Time (60::Word32) (0::Word32) False)
                `shouldBe` Time (61::Word32) (61::Word32) True
