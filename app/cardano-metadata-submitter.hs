{-# LANGUAGE ScopedTypeVariables #-}
import Cardano.Prelude
import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types
import Control.Arrow (left)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Prelude (String, id) -- XXX Why NoImplicitPrelude??
import qualified Data.ByteString.Lazy as B

data Arguments = Arguments
  { _ArgumentsFilename :: String
  , _ArgumentsBackupFile :: Maybe String
  , _ArgumentsRegistryEntry :: GoguenRegistryEntry Maybe
  }
  deriving Show

wellKnownOption :: forall p. WellKnownProperty p => (String -> String) -> OA.Mod OA.OptionFields (WellKnown p) -> OA.Parser (WellKnown p)
wellKnownOption strTransform opts = OA.option wellKnownReader opts where
  wellKnownReader :: OA.ReadM (WellKnown p)
  wellKnownReader = OA.eitherReader $ \str -> do
    pv :: PropertyValue <- left T.unpack $ propertyValueFromString $ T.pack $ strTransform str
    WellKnown pv <$> parseEither parseWellKnown pv

poorlyAttest :: a -> Attested a
poorlyAttest v = Attested [] v

withQuotes s = "\"" <> s <> "\"" -- XXX This is not at all the best way to do this part. I shall seek another.

argumentParser :: OA.Parser Arguments
argumentParser = Arguments <$>
  OA.strArgument (OA.metavar "FILENAME") <*>
  optional (OA.strOption (OA.long "backup" <> OA.short 'b' <> OA.metavar "BACKUP_FILE")) <*>
    (GoguenRegistryEntry
      <$> optional (Subject <$> OA.strOption (OA.long "subject" <> OA.short 's' <> OA.metavar "SUBJECT"))
      <*> optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
      <*> optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
      <*> pure Nothing -- XXX To write
    )

combineRegistryEntries :: GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
  { _goguenRegistryEntry_subject = _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
  , _goguenRegistryEntry_name = _goguenRegistryEntry_name new <|> _goguenRegistryEntry_name old
  , _goguenRegistryEntry_description = _goguenRegistryEntry_description new <|> _goguenRegistryEntry_description old
  , _goguenRegistryEntry_preimage = _goguenRegistryEntry_preimage new <|> _goguenRegistryEntry_preimage old
  }

main :: IO ()
main = do
    Arguments filename mBackup newEntryInfo <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty

    fileContents <- B.readFile filename

    WithOwnership owner record <- case decode fileContents of
        Just contents -> case parseEither parseRegistryEntry contents of
            Right res -> return res
            Left err -> do
                hPutStrLn stderr $ "Parse error: " <> err
                exitFailure
        Nothing -> do
            hPutStrLn stderr ("JSON parse error" :: String)
            exitFailure
    let newRecordWithOwnership = WithOwnership owner $ combineRegistryEntries newEntryInfo record

    forM_ mBackup $ \backupFilename -> do
      B.writeFile backupFilename fileContents

    writeFile filename $ show (serializeRegistryEntry newRecordWithOwnership) <> "\n"
    exitSuccess
