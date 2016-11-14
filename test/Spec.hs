import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import qualified Magento

tests :: TestTree
tests = testGroup "Tests" [unitTests]

magentoTests :: TestTree
magentoTests = testGroup "Magento"
  [ testCase "Get module namespace" $
    modNamespace `shouldBe` (Just "Foo")
  , testCase "Get module name" $
    modName `shouldBe` (Just "Bar")]
  where
    testModule = "Foo_Bar"
    testRoot = "/magento/root"
    modNamespace = Magento.getModuleNamespace testModule
    modName = Magento.getModuleName testModule
    baseDir = Magento.getBaseDir testRoot 


unitTests :: TestTree
unitTests = testGroup "Unit tests" [magentoTests]

main :: IO ()
main = defaultMain tests
