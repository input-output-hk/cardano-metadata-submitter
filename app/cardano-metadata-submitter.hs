{-# LANGUAGE ScopedTypeVariables #-}
import Cardano.Binary
import Cardano.Crypto.DSIGN
import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types
import Cardano.Prelude

import Control.Arrow (left)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Prelude (String, id)
import System.Directory

data DraftStatus = DraftStatusDraft | DraftStatusFinal
  deriving Show

data Arguments = Arguments
  { _ArgumentsFilename :: String
  , _ArgumentsSigningKeyFilename :: Maybe String
  , _ArgumentsFinalize :: DraftStatus
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

withQuotes :: String -> String
withQuotes s = "\"" <> s <> "\"" -- XXX This is not at all the best way to do this part. I shall seek another.

argumentParser :: OA.Parser Arguments
argumentParser = Arguments <$>
  OA.strArgument (OA.metavar "FILENAME") <*>
  optional (OA.strOption (OA.long "keyfile" <> OA.short 'k' <> OA.metavar "KEY_FILE")) <*>
  OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f') <*>
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
  Arguments filename skFname finalize newEntryInfo <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty
  let draftFilename = filename <> ".draft"

  registryJSONDraft <- eitherDecodeFileStrict draftFilename
  registryJSON <- case registryJSONDraft of
    Right res -> return res
    Left err -> eitherDecodeFileStrict filename

  WithOwnership owner record <- case registryJSON >>= parseEither parseRegistryEntry of
    Right res -> return res
    Left err -> do
        hPutStrLn stderr $ "Parse error: " <> err
        exitFailure
  let newRecordWithOwnership = WithOwnership owner $ combineRegistryEntries newEntryInfo record

  writeFile draftFilename $ show (serializeRegistryEntry newRecordWithOwnership) <> "\n"
  case finalize of
    DraftStatusFinal -> renameFile draftFilename filename
    DraftStatusDraft -> pure ()

  exitSuccess
