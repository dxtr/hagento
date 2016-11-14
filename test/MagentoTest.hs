module MagentoTest where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.Discover (Assertion, (@?=), TestTree, testCase)

import qualified Magento

tests :: TestTree
tests =
  testGroup "Magento" [ moduleTests ]

moduleTests :: TestTree
moduleTests = testGroup "Modules"
  [ testCase "Parse module namespace" modNamespace
  , testCase "Parse module name" modName
  ] 
  where
    modNamespace = Magento.getModuleNamespace module_ @?= ((Just "Foo") :: Maybe String)
    modName = Magento.getModuleName module_ @?= ((Just "Bar") :: Maybe String)


root :: String
root = "/magento/root"

module_ :: String
module_ = "Foo_Bar"

test_moduleParsing :: [TestTree]
test_moduleParsing =
  [ testCase "Parse module namespace" modNamespace
  , testCase "Parse module name" modName
  ]
  where
    modNamespace = Magento.getModuleNamespace module_ @?= ((Just "Foo") :: Maybe String)
    modName = Magento.getModuleName module_ @?= ((Just "Bar") :: Maybe String)

test_baseDir :: [TestTree]
test_baseDir =
  []

-- test_allTests :: [TestTree]
-- test_allTests =
--   [ testCase "Parse module namespace" case_modNamespace
--   , testCase "Parse module name" case_modName
--   ]


-- magentoTests :: TestTree
-- magentoTests = testGroup "Magento"
--   [ testCase "Get module namespace" $
--     modNamespace `shouldBe` (Just "Foo")
--   , testCase "Get module name" $
--     modName `shouldBe` (Just "Bar")]
--   where
--     testModule = "Foo_Bar"
--     testRoot = "/magento/root"
--     modNamespace = Magento.getModuleNamespace testModule
--     modName = Magento.getModuleName testModule
--     baseDir = Magento.getBaseDir testRoot 
