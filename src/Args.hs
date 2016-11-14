module Args
  ( Command(..)
  , Options(..)
  , argsParser
  ) where

import Options.Applicative

data Command
  = Xml
  | XmlListFiles
  | Admin
  | Cache
  | Cms
  | Config
  | Customer
  | Db
  | Design
  | Dev
  | Extension
  | Index
  | Sys
  | Help
  deriving Show

--data Verbosity = Normal | Verbose
data Options = Options { optDebug :: Bool
                       , optVerbose :: Bool
                       , optRoot :: String
                       , subCommand :: Command
                       } deriving Show

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

makeCommand :: String -> String -> Parser a -> Mod CommandFields a
makeCommand cmd desc p = command cmd (p `withInfo` desc)

parseCommand :: Parser Command
parseCommand = subparser $ xmlCmd <> helpCmd
  where
    xmlCmd = makeCommand "xml" "Do xml stuff" $
      subparser $ mconcat [ makeCommand "list-files" "List XML files" (pure XmlListFiles) ]
    helpCmd = makeCommand "help" "Halp!" (pure Help)

parseOptions :: Parser Options
parseOptions = Options
  <$> debugOpt
  <*> verboseOpt
  <*> rootOpt
  <*> parseCommand
  where
    debugOpt = switch $
      long "debug" <> short 'd' <> help "Debug mode"
    verboseOpt = switch $
      long "verbose" <> short 'v' <> help "Verbose mode"
    rootOpt = strOption $
      long "root" <> short 'r' <> metavar "ROOT" <> value "./"

parser :: ParserInfo Options
parser = info (helper <*> parseOptions) $
  mconcat [fullDesc, progDesc "Do Magento stuff", header "v0.0.1"]

argsParser :: (Options -> IO ()) -> IO ()
argsParser cmd = customExecParser (prefs showHelpOnError) parser >>= cmd
