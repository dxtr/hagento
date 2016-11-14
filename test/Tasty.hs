import Test.Tasty
import qualified MagentoTest as MT

tests :: TestTree
tests = testGroup "Tests" [MT.tests]

main :: IO ()
main = defaultMain tests
