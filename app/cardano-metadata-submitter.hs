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

data InputSource
  = InputSourceFile FileInfo
  | InputSourceStdin
  deriving Show

data FileInfo = FileInfo
  { _FileInfoFilename :: String
  , _FileInfoDraftStatus :: DraftStatus
  }
  deriving Show

data Arguments = Arguments
  { _ArgumentsInputSource :: InputSource
  , _ArgumentsSigningKeyFilename :: Maybe String
  , _ArgumentsRegistryEntry :: PartialGoguenRegistryEntry
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
  inputSourceArgumentParser <*>
  optional (OA.strOption (OA.long "keyfile" <> OA.short 'k' <> OA.metavar "KEY_FILE")) <*>
  goguenRegistryEntryParser where
    inputSourceArgumentParser :: OA.Parser InputSource
    inputSourceArgumentParser = OA.flag' InputSourceStdin (OA.long "stdin" <> OA.short 'i') <|>
      (InputSourceFile <$> fileInfoArgumentParser)

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo <$>
      OA.strArgument (OA.metavar "FILENAME") <*>
      OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry <$>
      optional (Subject <$> OA.strOption (OA.long "subject" <> OA.short 's' <> OA.metavar "SUBJECT")) <*>
      optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME")) <*>
      optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION")) <*>
      pure Nothing -- XXX To write

combineRegistryEntries :: GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
  { _goguenRegistryEntry_subject = _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
  , _goguenRegistryEntry_name = _goguenRegistryEntry_name new <|> _goguenRegistryEntry_name old
  , _goguenRegistryEntry_description = _goguenRegistryEntry_description new <|> _goguenRegistryEntry_description old
  , _goguenRegistryEntry_preimage = _goguenRegistryEntry_preimage new <|> _goguenRegistryEntry_preimage old
  }

main :: IO ()
main = do
  Arguments inputInfo mskFname newEntryInfo <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty

  key :: Maybe (SignKeyDSIGN Ed25519DSIGN) <- forM mskFname $ \skFname -> do
    lbs <- B.readFile skFname
    dieOnLeft "Error reading key file" $ left show $ decodeFullDecoder "Signing Key" decodeSignKeyDSIGN lbs

  let draftFilename (FileInfo fn _) = fn <> ".draft"

  registryJSON <- case inputInfo of
    InputSourceFile fInfo -> do
      let dfn = draftFilename fInfo
      exists <- doesFileExist $ draftFilename fInfo
      let readFn = if exists then dfn else _FileInfoFilename fInfo
      eitherDecodeFileStrict readFn
    InputSourceStdin -> do
      input <- B.getContents
      pure $ eitherDecode input

  WithOwnership owner record <- dieOnLeft "Parse error" $ do
    json <- registryJSON
    parseEither parseRegistryEntry json

  let newRecordWithOwnership = WithOwnership owner $ combineRegistryEntries newEntryInfo record
      outputString = show (serializeRegistryEntry newRecordWithOwnership) <> "\n"

  case inputInfo of
    InputSourceFile fInfo -> do
      writeFile (draftFilename fInfo) outputString
      case _FileInfoDraftStatus fInfo of
        DraftStatusFinal -> renameFile (draftFilename fInfo) $ _FileInfoFilename fInfo
        DraftStatusDraft -> pure ()
    InputSourceStdin -> do
      putStr outputString

  exitSuccess
  where
    dieOnLeft :: String -> Either String a -> IO a
    dieOnLeft lbl eVal = case eVal of
      Left err -> die $ T.pack $ lbl <> ": " <> err
      Right val -> pure val
