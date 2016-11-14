module Lib
    ( hagentoMain
    ) where

import Args (Options(..))
import qualified Args
import qualified Xml
import qualified Magento

hagentoMain :: IO ()
hagentoMain = Args.argsParser run

run :: Options -> IO ()
run opts@Options { optRoot = root, subCommand = Args.XmlListFiles } = do
  putStrLn $ show opts
  files <- Xml.getXmlFiles root
  putStrLn $ show files
  --map putStrLn files
