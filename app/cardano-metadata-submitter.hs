{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import Cardano.Binary
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Seed
import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types
import Cardano.Prelude

import Control.Arrow (left)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Prelude (String, id)
import System.Directory (doesFileExist, renameFile)
import System.Environment (lookupEnv)

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

data AttestationField
  = AttestationFieldName
  | AttestationFieldDescription
  deriving (Show, Eq, Ord)

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

data EntryUpdateArguments = EntryUpdateArguments
  { _EntryUpdateArgumentsInputSource :: InputSource
  , _EntryUpdateArgumentsAttestationKeyFilename :: Maybe String
  , _EntryUpdateArgumentsAttestationFields :: [AttestationField]
  , _EntryUpdateArgumentsOwnershipKeyFilename :: Maybe String
  , _EntryUpdateArgumentsRegistryEntry :: PartialGoguenRegistryEntry
  }
  deriving Show

data KeyGenerationArguments = KeyGenerationArguments
  { _keyGenerationArgumentsFileName :: String
  }
  deriving Show

data Arguments
  = ArgumentsEntryUpdate EntryUpdateArguments
  | ArgumentsKeyGeneration KeyGenerationArguments
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
withQuotes s = B8.unpack $ A.encode $ A.String $ T.pack s

entryUpdateArgumentParser :: Maybe String -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments <$>
  inputSourceArgumentParser <*>
  optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE")) <*>
  attestationFieldNamesParser <*>
  optional (OA.strOption (OA.long "owner-keyfile" <> OA.short 'o' <> OA.metavar "OWNER_KEY_FILE")) <*>
  goguenRegistryEntryParser where
    inputSourceArgumentParser :: OA.Parser InputSource
    inputSourceArgumentParser = OA.flag' InputSourceStdin (OA.long "stdin" <> OA.short 'I') <|>
      (InputSourceFile <$> fileInfoArgumentParser)

    attestationFieldNamesParser :: OA.Parser [AttestationField]
    attestationFieldNamesParser =
      OA.flag' [AttestationFieldName] (OA.long "attest-name" <> OA.short 'N') <|>
      OA.flag' [AttestationFieldDescription] (OA.long "attest-description" <> OA.short 'D') <|>
      pure [AttestationFieldName, AttestationFieldDescription]

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo <$>
      (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT") <|> defaultSubjectParser) <*>
      OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i') <*>
      OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    defaultSubjectParser = case defaultSubject of
      Just subj -> pure subj
      Nothing -> empty

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
    preimageParser = do
      hashFn <- T.pack <$> OA.strOption (OA.long "hashFn" <> OA.short 'H' <> OA.metavar "HASH_FUNCTION")
      preimage <- T.pack <$> OA.strOption (OA.long "preimage" <> OA.short 'p' <> OA.metavar "PREIMAGE")
      pure $ Preimage
        { _preimage_hashFn = hashFn
        , _preimage_preimage = preimage
        }

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

attestFields :: SignKeyDSIGN Ed25519DSIGN -> [AttestationField] -> PartialGoguenRegistryEntry -> Either String PartialGoguenRegistryEntry
attestFields key props old = do
  subj <- case _goguenRegistryEntry_subject old of
    Just subj -> pure subj
    Nothing -> Left "Cannot attest without a subject record"
  pure $ old
    { _goguenRegistryEntry_name = attestField AttestationFieldName subj <$> _goguenRegistryEntry_name old
    , _goguenRegistryEntry_description = attestField AttestationFieldDescription subj <$> _goguenRegistryEntry_description old
    }
  where
    attestField :: WellKnownProperty p => AttestationField -> Subject -> Attested (WellKnown p) -> Attested (WellKnown p)
    attestField fld subj (Attested att wk@(WellKnown raw structed)) = if fld `elem` props
      then Attested attestations wk
      else Attested att wk where
        wkHash = hashesForAttestation subj (wellKnownPropertyName (Identity structed)) raw
        newAttestationSig = makeAttestationSignature key wkHash
        attestations = newAttestationSig:att

ownerSignature :: SignKeyDSIGN Ed25519DSIGN -> PartialGoguenRegistryEntry -> Either String OwnershipSignature
ownerSignature key reg = makeOwnershipSignature key <$> hashes where
  hashes = do
    subj <- asEither "subject" $ _goguenRegistryEntry_subject reg
    name <- asEither "name" $ _goguenRegistryEntry_name reg
    description <- asEither "description" $ _goguenRegistryEntry_description reg
    pure $ registryHashesForOwnership subj $ Map.fromList
      [ withWellKnown (_attested_property name) $ \p _ -> (p, _wellKnown_raw <$> name)
      , withWellKnown (_attested_property description) $ \p _ -> (p, _wellKnown_raw <$> description)
      ]

  asEither _ (Just x) = Right x
  asEither s Nothing = Left $ "cannot hash with missing " <> s

handleEntryUpdateArguments :: EntryUpdateArguments -> IO ()
handleEntryUpdateArguments (EntryUpdateArguments inputInfo attestKeyFile attestProps ownerKeyFile newEntryInfo) = do
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
    Just k -> attestFields k attestProps newRecord
    Nothing -> pure newRecord

  newOwner <- dieOnLeft "Adding owner signature" $ case ownerKey of
    Just k -> Just <$> ownerSignature k newRecordWithAttestations
    Nothing -> pure oldOwner

  let newRecordWithOwnership = WithOwnership newOwner newRecordWithAttestations
      outputString = show (serializeRegistryEntry newRecordWithOwnership) <> "\n"

  case _goguenRegistryEntry_preimage newRecordWithAttestations of
    Nothing -> pure ()
    Just preimage -> dieOnLeft "Checking preimage" $ do
      subject <- maybe (Left "Subject missing") Right $ _goguenRegistryEntry_subject newRecordWithAttestations
      verifyPreimage subject preimage

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

keyGenerationParser :: OA.Parser KeyGenerationArguments
keyGenerationParser = KeyGenerationArguments <$>
  OA.strOption (OA.long "generate-key" <> OA.short 'K' <> OA.metavar "KEY")


handleKeyGeneration :: KeyGenerationArguments -> IO ()
handleKeyGeneration (KeyGenerationArguments fname) = do
  let pubKeyname = fname <> ".pub"
      privKeyName = fname <> ".prv"

      seedSize = seedSizeDSIGN (Proxy @Ed25519DSIGN)

  seed <- readSeedFromSystemEntropy seedSize

  let signKey = genKeyDSIGN seed
      pubKey = deriveVerKeyDSIGN @Ed25519DSIGN signKey

  writeKeyFile pubKeyname $ encodeVerKeyDSIGN @Ed25519DSIGN pubKey
  writeKeyFile privKeyName $ encodeSignKeyDSIGN @Ed25519DSIGN signKey
  where
    writeKeyFile :: FilePath -> Encoding -> IO ()
    writeKeyFile fname enc = do
      exists <- doesFileExist fname
      if exists
        then die $ T.pack $ "File already exists: " <> fname
        else B.writeFile fname $ serializeEncoding enc

argumentParser :: Maybe String -> OA.Parser Arguments
argumentParser defaultSubject = (ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject) <|>
  (ArgumentsKeyGeneration <$> keyGenerationParser)

main :: IO ()
main = do
  defaultSubject :: Maybe String <- lookupEnv "METADATA_SUBJECT"
  args <- OA.execParser $ OA.info (argumentParser defaultSubject <**> OA.helper) mempty
  case args of
    ArgumentsEntryUpdate eua -> handleEntryUpdateArguments eua
    ArgumentsKeyGeneration ka -> handleKeyGeneration ka
