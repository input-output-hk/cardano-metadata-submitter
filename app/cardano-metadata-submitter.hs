{-# LANGUAGE ScopedTypeVariables #-}
import Cardano.Binary
import Cardano.Crypto.DSIGN
import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types
import Cardano.Prelude

import Control.Arrow (left)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Prelude (String, id)
import System.Directory (doesFileExist, renameFile)

data DraftStatus = DraftStatusDraft | DraftStatusFinal
  deriving Show

data InputSource
  = InputSourceFile FileInfo
  | InputSourceStdin
  deriving Show

data EntryOperation
  = EntryOperationInitialize
  | EntryOperationRevise
  deriving Show

data FileInfo = FileInfo
  { _FileInfoSubject :: String
  , _FileInfoEntryOperation :: EntryOperation
  , _FileInfoDraftStatus :: DraftStatus
  }
  deriving Show

canonicalFilename :: FileInfo -> String
canonicalFilename fi = _FileInfoSubject fi <> ".json"

draftFilename :: FileInfo -> String
draftFilename fi = canonicalFilename fi <> ".draft"

data Arguments = Arguments
  { _ArgumentsInputSource :: InputSource
  , _ArgumentsAttestationKeyFilename :: Maybe String
  , _ArgumentsOwnershipKeyFilename :: Maybe String
  , _ArgumentsRegistryEntry :: PartialGoguenRegistryEntry
  }
  deriving Show

wellKnownOption :: forall p. WellKnownProperty p => (String -> String) -> OA.Mod OA.OptionFields (WellKnown p) -> OA.Parser (WellKnown p)
wellKnownOption strTransform opts = OA.option wellKnownReader opts where
  wellKnownReader :: OA.ReadM (WellKnown p)
  wellKnownReader = OA.eitherReader $ \str -> do
    pv :: PropertyValue <- left T.unpack $ propertyValueFromString $ T.pack $ strTransform str
    WellKnown pv <$> A.parseEither parseWellKnown pv

poorlyAttest :: a -> Attested a
poorlyAttest v = Attested [] v

withQuotes :: String -> String
withQuotes s = "\"" <> s <> "\"" -- XXX This is not at all the best way to do this part. I shall seek another.

argumentParser :: OA.Parser Arguments
argumentParser = Arguments <$>
  inputSourceArgumentParser <*>
  optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE")) <*>
  optional (OA.strOption (OA.long "owner-keyfile" <> OA.short 'o' <> OA.metavar "OWNER_KEY_FILE")) <*>
  goguenRegistryEntryParser where
    inputSourceArgumentParser :: OA.Parser InputSource
    inputSourceArgumentParser = OA.flag' InputSourceStdin (OA.long "stdin" <> OA.short 'I') <|>
      (InputSourceFile <$> fileInfoArgumentParser)

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo <$>
      (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT")) <*>
      OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i') <*>
      OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    trimSubject :: String -> String
    trimSubject subj = if ".json" `isSuffixOf` subj
      then take (length subj - 5) subj
      else subj

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry <$>
      pure Nothing <*>
      optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME")) <*>
      optional (poorlyAttest <$> wellKnownOption withQuotes (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION")) <*>
      optional preimageParser

    preimageParser :: OA.Parser Preimage
    preimageParser = Preimage <$>
      (T.pack <$> OA.strOption (OA.long "hashFn" <> OA.short 'H' <> OA.metavar "HASH_FUNCTION")) <*>
      (T.pack <$> OA.strOption (OA.long "preimage" <> OA.short 'p' <> OA.metavar "PREIMAGE"))

combineRegistryEntries :: GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
  { _goguenRegistryEntry_subject = _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
  , _goguenRegistryEntry_name = _goguenRegistryEntry_name new `combineAttestedEntry` _goguenRegistryEntry_name old
  , _goguenRegistryEntry_description = _goguenRegistryEntry_description new `combineAttestedEntry` _goguenRegistryEntry_description old
  , _goguenRegistryEntry_preimage = _goguenRegistryEntry_preimage new <|> _goguenRegistryEntry_preimage old
  } where
    combineAttestedEntry a b = case (a, b) of
      (Just (Attested sigA valA), Just (Attested sigB valB)) | raw valA == raw valB -> Just $ Attested (sigA ++ sigB) valA
      _ -> a <|> b
    raw (WellKnown (PropertyValue r _) _) = r

attestField :: WellKnownProperty p => SignKeyDSIGN Ed25519DSIGN -> Subject -> Attested (WellKnown p) -> Attested (WellKnown p)
attestField key subj (Attested att wk@(WellKnown raw structed)) = Attested attestations wk where
  wkHash = hashesForAttestation subj (wellKnownPropertyName (Identity structed)) raw
  newAttestationSig = makeAttestationSignature key wkHash
  attestations = newAttestationSig:att

attestFields :: SignKeyDSIGN Ed25519DSIGN -> PartialGoguenRegistryEntry -> Either String PartialGoguenRegistryEntry
attestFields key old = do
  subj <- case _goguenRegistryEntry_subject old of
    Just subj -> pure subj
    Nothing -> Left "Cannot attest without a subject record"
  pure $ old
    { _goguenRegistryEntry_name = attestField key subj <$> _goguenRegistryEntry_name old
    , _goguenRegistryEntry_description = attestField key subj <$> _goguenRegistryEntry_description old
    }

ownerSignature :: SignKeyDSIGN Ed25519DSIGN -> PartialGoguenRegistryEntry -> Either String OwnershipSignature
ownerSignature key reg = Left "Ownership signatures not yet supported" -- XXX

main :: IO ()
main = do
  Arguments inputInfo attestKeyFile ownerKeyFile newEntryInfo <- OA.execParser $ OA.info (argumentParser <**> OA.helper) mempty

  attestKey <- mapM readKeyFile attestKeyFile
  ownerKey <- mapM readKeyFile ownerKeyFile

  WithOwnership oldOwner record <- case inputInfo of
    InputSourceStdin -> do
      input <- B.getContents
      parseJSON $ A.eitherDecode input
    InputSourceFile fInfo@(FileInfo _ EntryOperationRevise _) -> do
      let dfn = draftFilename fInfo
      exists <- doesFileExist $ draftFilename fInfo
      let readFn = if exists then dfn else canonicalFilename fInfo
      json <- A.eitherDecodeFileStrict readFn
      parseJSON json
    InputSourceFile (FileInfo subj EntryOperationInitialize _) -> do
      pure $ WithOwnership Nothing $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Just $ Subject $ T.pack subj
        , _goguenRegistryEntry_name = Nothing
        , _goguenRegistryEntry_description = Nothing
        , _goguenRegistryEntry_preimage = Nothing
        }

  let newRecord = combineRegistryEntries newEntryInfo record

  newRecordWithAttestations <- dieOnLeft "Adding attestation" $ case attestKey of
    Just k -> attestFields k newRecord
    Nothing -> pure newRecord

  newOwner <- dieOnLeft "Adding owner signature" $ case ownerKey of
    Just k -> Just <$> ownerSignature k newRecordWithAttestations
    Nothing -> pure oldOwner

  let newRecordWithOwnership = WithOwnership newOwner newRecordWithAttestations
      outputString = show (serializeRegistryEntry newRecordWithOwnership) <> "\n"

  case inputInfo of
    InputSourceFile fInfo -> do
      writeFile (draftFilename fInfo) outputString
      case _FileInfoDraftStatus fInfo of
        DraftStatusFinal -> renameFile (draftFilename fInfo) $ canonicalFilename fInfo
        DraftStatusDraft -> pure ()
    InputSourceStdin -> do
      putStr outputString

  exitSuccess
  where
    dieOnLeft :: String -> Either String a -> IO a
    dieOnLeft lbl eVal = case eVal of
      Left err -> die $ T.pack $ lbl <> ": " <> err
      Right val -> pure val

    readKeyFile :: FilePath -> IO (SignKeyDSIGN Ed25519DSIGN)
    readKeyFile skFname = do
      lbs <- B.readFile skFname
      dieOnLeft "Error reading key file" $ left show $ decodeFullDecoder "Signing Key" decodeSignKeyDSIGN lbs

    parseJSON :: Either String A.Value -> IO (WithOwnership Maybe PartialGoguenRegistryEntry)
    parseJSON registryJSON = dieOnLeft "Parse error" $ do
      json <- registryJSON
      A.parseEither parseRegistryEntry json
