--{-# OPTIONS_GHC -F -pgmF tasty-discover #-}

import Test.Tasty
-- --import Test.Tasty.SmallCheck as SC
-- --import Test.Tasty.QuickCheck as QC
-- --import Test.Tasty.HUnit
-- --import Test.Tasty.Hspec

--import qualified Magento
import qualified MagentoTest as MT

tests :: TestTree
tests = testGroup "Tests" [MT.tests]

-- unitTests :: TestTree
-- unitTests = testGroup "Unit tests" [magentoTests]

main :: IO ()
main = defaultMain tests
