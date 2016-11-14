import Test.Tasty
import qualified MagentoTest as MT

tests :: TestTree
tests = testGroup "" [MT.tests]

main :: IO ()
main = defaultMain tests
