module MagentoTest where

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
--import Test.Tasty.Discover (Assertion, (@?=), TestTree, testCase)

import qualified Magento

root :: String
root = "/magento/root"

module_ :: String
module_ = "Foo_Bar"

tests :: TestTree
tests = testGroup "Magento" [ moduleTests
                            , baseDirTests
                            ]

moduleTests :: TestTree
moduleTests = testGroup "Modules"
  [ testCase "Parse module namespace" modNamespace
  , testCase "Parse module name" modName
  , testCase "Get module directory (core)" coreModule
  , testCase "Get module directory (community)" communityModule
  , testCase "Get module directory (local)" localModule
  ] 
  where
    modNamespace = Magento.getModuleNamespace module_ @?= ((Just "Foo") :: Maybe String)
    modName = Magento.getModuleName module_ @?= ((Just "Bar") :: Maybe String)
    modulePath cp m = root ++ "/app/code/" ++ cp ++ "/" ++ (fromJust $ Magento.getModuleNamespace m) ++ "/" ++ (fromJust $ Magento.getModuleName m)
    caseModuleDir cp = Magento.getModuleDir root cp module_ @?= ((Just (modulePath cp module_)) :: Maybe FilePath)
    
    coreModule = caseModuleDir "core"
    communityModule = caseModuleDir "community"
    localModule = caseModuleDir "local"

baseDirTests :: TestTree
baseDirTests = testGroup "Base Dir" cases
  where
    funcBaseDir d extra = Magento.getBaseDir root d @?= ((Just (root ++ extra ++ d)) :: Maybe FilePath)
    caseBase f l = [testCase (printf "getBaseDir \"%s\"" name) (f name) | name <- l]
    funcBaseBaseDir d = funcBaseDir d "/"
    funcBaseAppDir d = funcBaseDir d "/app/"
    funcBaseVarDir d = funcBaseDir d "/var/"
    funcBaseMediaDir d = funcBaseDir d "/media/"

    caseBaseBaseDir = caseBase funcBaseBaseDir ["app", "lib", "media", "skin", "var"]
    caseBaseAppDir = caseBase funcBaseAppDir ["code", "design", "etc", "locale"]
    caseBaseVarDir = caseBase funcBaseVarDir ["tmp", "cache", "log", "session", "export"]
    caseBaseMediaDir = caseBase funcBaseMediaDir ["upload"]
    cases = caseBaseBaseDir ++ caseBaseAppDir ++ caseBaseVarDir ++ caseBaseMediaDir
