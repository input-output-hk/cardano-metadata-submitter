{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Prelude

import Cardano.Binary
    ( Encoding, decodeFullDecoder, serializeEncoding )
import Cardano.Crypto.DSIGN
    ( Ed25519DSIGN
    , SignKeyDSIGN
    , decodeSignKeyDSIGN
    , deriveVerKeyDSIGN
    , encodeSignKeyDSIGN
    , encodeVerKeyDSIGN
    , genKeyDSIGN
    , seedSizeDSIGN
    )
import Cardano.Crypto.Seed
    ( readSeedFromSystemEntropy )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    , hashPreimage
    , parseRegistryEntry
    , registryHashesForOwnership
    , serializeRegistryEntry
    , verifyPreimage
    , verifyRegistryOwnership
    )
import Cardano.Metadata.Types
    ( Attested (..)
    , HashesForAttestation (..)
    , Logo (..)
    , OwnershipSignature (..)
    , Preimage (..)
    , PropertyValue (..)
    , Subject (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , WithOwnership (..)
    , emptyAttested
    , hashesForAttestation
    , makeAttestationSignature
    , makeOwnershipSignature
    , propertyValueFromString
    , verifyAttested
    , withWellKnown
    )
import Control.Arrow
    ( left )
import Data.List
    ( isSuffixOf )
import Prelude
    ( String )
import System.Directory
    ( doesFileExist, renameFile )
import System.Environment
    ( lookupEnv )
import Text.Hex
    ( encodeHex )

import Codec.Picture.Png
    ( decodePng )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Options.Applicative as OA

main :: IO ()
main = do
    defaultSubject :: Maybe String <- lookupEnv "METADATA_SUBJECT"
    args <- OA.execParser $ OA.info (argumentParser defaultSubject <**> OA.helper) mempty
    case args of
        ArgumentsEntryUpdate eua  -> handleEntryUpdateArguments eua
        ArgumentsKeyGeneration ka -> handleKeyGeneration ka

data DraftStatus
    = DraftStatusDraft
    | DraftStatusFinal
    deriving Show

data InputSource
    = InputSourceFile FileInfo
    | InputSourceStdin
    | InputSourceCBORJson CBORInfo
    deriving Show

data CBORInfo = CBORInfo
  { _CBORInfoFilename :: String
  , _CBORInfoHashFunction :: String
  } deriving Show

data EntryOperation
    = EntryOperationInitialize
    | EntryOperationRevise
    deriving Show

data AttestationField
    = AttestationFieldName
    | AttestationFieldDescription
    | AttestationFieldLogo
    deriving (Show, Eq, Ord)

data FileInfo = FileInfo
    { _FileInfoSubject :: String
    , _FileInfoTokenName :: Maybe String
    , _FileInfoEntryOperation :: EntryOperation
    , _FileInfoDraftStatus :: DraftStatus
    }
  deriving Show

fullSubject :: FileInfo -> Subject
fullSubject fi = Subject $ T.pack $ _FileInfoSubject fi <> tokenAsHex where
  tokenAsHex = fromMaybe "" $ T.unpack . encodeHex . BL.toStrict . B8.pack <$> _FileInfoTokenName fi

canonicalFilename :: FileInfo -> String
canonicalFilename fi = case fullSubject fi of
  Subject txt -> T.unpack txt <> jsonSuffix

jsonSuffix, draftSuffix, jsonDraftSuffix :: String
jsonSuffix = ".json"
draftSuffix = ".draft"
jsonDraftSuffix = jsonSuffix <> draftSuffix

draftFilename :: FileInfo -> String
draftFilename fi = canonicalFilename fi <> draftSuffix

data EntryUpdateArguments = EntryUpdateArguments
    { _EntryUpdateArgumentsInputSource :: InputSource
    , _EntryUpdateArgumentsAttestationKeyFilename :: Maybe String
    , _EntryUpdateArgumentsAttestationFields :: [AttestationField]
    , _EntryUpdateArgumentsOwnershipKeyFilename :: Maybe String
    , _EntryUpdateArgumentsRegistryEntry :: PartialGoguenRegistryEntry
    , _EntryUpdateLogoFilename :: Maybe String
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

wellKnownOption
  :: forall p. WellKnownProperty p
  => (String -> String)
  -> OA.Mod OA.OptionFields (WellKnown p)
  -> OA.Parser (WellKnown p)
wellKnownOption strTransform =
    OA.option wellKnownReader
  where
    wellKnownReader :: OA.ReadM (WellKnown p)
    wellKnownReader = OA.eitherReader $ \str -> do
        pv :: PropertyValue <- left T.unpack $ propertyValueFromString $ T.pack $ strTransform str
        WellKnown pv <$> Aeson.parseEither parseWellKnown pv

withQuotes :: String -> String
withQuotes s = B8.unpack $ Aeson.encode $ Aeson.String $ T.pack s

entryUpdateArgumentParser :: Maybe String -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments
    <$> inputSourceArgumentParser
    <*> optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE"))
    <*> attestationFieldNamesParser
    <*> optional (OA.strOption (OA.long "owner-keyfile" <> OA.short 'o' <> OA.metavar "OWNER_KEY_FILE"))
    <*> goguenRegistryEntryParser
    <*> logoFilenameParser
  where
    inputSourceArgumentParser :: OA.Parser InputSource
    inputSourceArgumentParser = asum
        [ OA.flag' InputSourceStdin $ OA.long "stdin" <> OA.short 'I'
        , InputSourceFile <$> fileInfoArgumentParser
        , InputSourceCBORJson <$> cborInfoArgumentParser
        ]

    attestationFieldNamesParser :: OA.Parser [AttestationField]
    attestationFieldNamesParser = asum
       [ OA.flag' [AttestationFieldName] $ OA.long "attest-name" <> OA.short 'N'
       , OA.flag' [AttestationFieldDescription] $ OA.long "attest-description" <> OA.short 'D'
       , OA.flag' [AttestationFieldLogo] $ OA.long "attest-logo" <> OA.short 'L'
       , pure [AttestationFieldName, AttestationFieldDescription, AttestationFieldLogo]
       ]

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo
        <$> (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT") <|> defaultSubjectParser)
        <*> tokenNameParser
        <*> OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i')
        <*> OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    cborInfoArgumentParser = CBORInfo
        <$> OA.strOption (OA.long "cbor-info" <> OA.short 'c' <> OA.metavar "CBOR_INFO_FILENAME")
        <*> OA.strOption (OA.long "hash-function" <> OA.short 'H' <> OA.metavar "HASH_FUNCTION")

    tokenNameParser = optional $ OA.strOption (OA.long "token-name" <> OA.short 't' <> OA.metavar "TOKEN_NAME")

    defaultSubjectParser =
        maybe empty pure defaultSubject

    trimSubject :: String -> String
    trimSubject subj
        | jsonSuffix `isSuffixOf` subj =
            take (length subj - length jsonSuffix) subj
        | jsonDraftSuffix `isSuffixOf` subj =
            take (length subj - length jsonDraftSuffix) subj
        | otherwise =
            subj

    logoFilenameParser :: OA.Parser (Maybe String)
    logoFilenameParser = optional $ OA.strOption (OA.long "logo" <> OA.short 'l' <> OA.metavar "LOGO.png")

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry
        <$> pure Nothing
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
        <*> pure Nothing
        <*> optional preimageParser

    preimageParser :: OA.Parser Preimage
    preimageParser = do
        hashFn <- T.pack <$> OA.strOption (OA.long "hashFn" <> OA.short 'H' <> OA.metavar "HASH_FUNCTION")
        preimage <- T.pack <$> OA.strOption (OA.long "preimage" <> OA.short 'p' <> OA.metavar "PREIMAGE")
        pure $ Preimage
            { _preimage_hashFn = hashFn
            , _preimage_preimage = preimage
            }

combineRegistryEntries
    :: GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
    { _goguenRegistryEntry_subject =
        _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
    , _goguenRegistryEntry_name =
        _goguenRegistryEntry_name new `combineAttestedEntry` _goguenRegistryEntry_name old
    , _goguenRegistryEntry_description =
        _goguenRegistryEntry_description new `combineAttestedEntry` _goguenRegistryEntry_description old
    , _goguenRegistryEntry_logo =
        _goguenRegistryEntry_logo new `combineAttestedEntry` _goguenRegistryEntry_logo old
    , _goguenRegistryEntry_preimage =
        _goguenRegistryEntry_preimage new <|> _goguenRegistryEntry_preimage old
    }
  where
    combineAttestedEntry a b = case (a, b) of
        (Just (Attested sigA valA), Just (Attested sigB valB)) | raw valA == raw valB ->
            Just $ Attested (sigA ++ sigB) valA
        _ ->
            a <|> b
    raw (WellKnown (PropertyValue r _) _) = r

attestFields
    :: SignKeyDSIGN Ed25519DSIGN
    -> [AttestationField]
    -> PartialGoguenRegistryEntry
    -> Either String PartialGoguenRegistryEntry
attestFields key props old = do
    subj <- case _goguenRegistryEntry_subject old of
        Just subj -> pure subj
        Nothing -> Left "Cannot attest without a subject record"
    pure $ old
        { _goguenRegistryEntry_name =
            attestField AttestationFieldName subj <$> _goguenRegistryEntry_name old
        , _goguenRegistryEntry_description =
            attestField AttestationFieldDescription subj <$> _goguenRegistryEntry_description old
        , _goguenRegistryEntry_logo =
            attestField AttestationFieldLogo subj <$> _goguenRegistryEntry_logo old
        }
  where
    attestField
        :: WellKnownProperty p
        => AttestationField
        -> Subject
        -> Attested (WellKnown p)
        -> Attested (WellKnown p)
    attestField fld subj orig@(Attested att wk) =
        if fld `elem` props
        then Attested attestations wk
        else Attested att wk
      where
        wkHash = extractAttestationHashes subj orig
        newAttestationSig = makeAttestationSignature key wkHash
        attestations = newAttestationSig:att

extractAttestationHashes
    :: WellKnownProperty p
    => Subject
    -> Attested (WellKnown p)
    -> HashesForAttestation
extractAttestationHashes subj (Attested _ (WellKnown raw structured)) =
    hashesForAttestation subj (wellKnownPropertyName (Identity structured)) raw

ownerSignature
    :: SignKeyDSIGN Ed25519DSIGN
    -> PartialGoguenRegistryEntry
    -> Either String OwnershipSignature
ownerSignature key reg =
    makeOwnershipSignature key <$> hashes
  where
    hashes = do
        subj <- asEither "subject" $ _goguenRegistryEntry_subject reg
        name <- asEither "name" $ _goguenRegistryEntry_name reg
        description <- asEither "description" $ _goguenRegistryEntry_description reg
        pure $ registryHashesForOwnership subj $ Map.fromList
            [ withWellKnown (_attested_property name) $ \p _ ->
                (p, _wellKnown_raw <$> name)
            , withWellKnown (_attested_property description) $ \p _ ->
                (p, _wellKnown_raw <$> description)
            ]

    asEither _ (Just x) = Right x
    asEither s Nothing = Left $ "cannot hash with missing " <> s

verifyEverything
    :: WithOwnership Maybe PartialGoguenRegistryEntry
    -> Either String ()
verifyEverything record = do
    -- these fields are mandatory
    subj <- verifyField _goguenRegistryEntry_subject
    name <- verifyField _goguenRegistryEntry_name
    desc <- verifyField _goguenRegistryEntry_description
    case _withOwnership_owner record of
        Nothing -> pure () -- It's OK if we have no owner
        Just _ -> do   -- but if we do, verify the signature
            left ("Ownership signature verifiction failed: " <>) $ verifyRegistryOwnership record

    -- Similar to owner signature, this only verifies present attestations. If no attestations are present, it passes.
    let verifyLocalAttestations fieldName field = do
            let hashes = extractAttestationHashes subj field
            let (Attested attestations _) = field
            left (const $ fieldName <> " attestation verification failed") $
                verifyAttested $ Attested attestations hashes

    verifyLocalAttestations "Name" name
    verifyLocalAttestations "Description" desc
    forM_ (_goguenRegistryEntry_logo $ _withOwnership_value record) $ \logo -> do
        verifyLocalAttestations "Logo" logo
        let (Attested _ (WellKnown _ (Logo logoData))) = logo
        void $ left ("Verifying PNG: " <>) $ decodePng $ BL.toStrict logoData
  -- preimage is not attested as it must hash to the subject
  where
    verifyField :: (PartialGoguenRegistryEntry -> Maybe a) -> Either String a
    verifyField field = maybe
        (Left missingFields)
        Right
        (field $ _withOwnership_value record)

    missingFields = concat
        [ missingField "Missing field subject" _goguenRegistryEntry_subject
        , missingField "Missing field name: Use -n to specify" _goguenRegistryEntry_name
        , missingField "Missing field description: Use -d to specify" _goguenRegistryEntry_description
        ]

    missingField str fld = case fld $ _withOwnership_value record of
        Just _ -> ""
        Nothing -> "\n" <> str

handleEntryUpdateArguments :: EntryUpdateArguments -> IO ()
handleEntryUpdateArguments (EntryUpdateArguments inputInfo attestKeyFile attestProps ownerKeyFile newEntryInfo logoFname) = do
    attestKey <- mapM readKeyFile attestKeyFile
    ownerKey <- mapM readKeyFile ownerKeyFile

    WithOwnership oldOwner record <- case inputInfo of
        InputSourceStdin -> do
            input <- BL.getContents
            parseJSON $ Aeson.eitherDecode input
        InputSourceCBORJson (CBORInfo fname hashFn) -> do
            inputOrError <- Aeson.eitherDecodeFileStrict fname
            preimage <- dieOnLeft "Parsing CBOR data" $ inputOrError >>= \case
                Aeson.Object obj -> do
                    case HM.lookup "cborHex" obj of
                        Just (Aeson.String txt) -> Right txt
                        _ -> Left "JSON misformatted"
                _ -> Left "JSON contained no object"
            let preimageWithFn = Preimage
                    { _preimage_hashFn = T.pack hashFn
                    , _preimage_preimage = preimage
                    }
            subject <- dieOnLeft "Hashing preimage" $ hashPreimage preimageWithFn
            pure $ WithOwnership Nothing $ GoguenRegistryEntry
                { _goguenRegistryEntry_subject = Just subject
                , _goguenRegistryEntry_name = Nothing
                , _goguenRegistryEntry_description = Nothing
                , _goguenRegistryEntry_logo = Nothing
                , _goguenRegistryEntry_preimage = Just preimageWithFn
                }
        InputSourceFile fInfo -> case _FileInfoEntryOperation fInfo of
            EntryOperationRevise -> do
                let dfn = draftFilename fInfo
                exists <- doesFileExist $ draftFilename fInfo
                let readFn = if exists then dfn else canonicalFilename fInfo
                json <- Aeson.eitherDecodeFileStrict readFn
                parseJSON json
            EntryOperationInitialize -> pure $ WithOwnership Nothing $ GoguenRegistryEntry
                { _goguenRegistryEntry_subject = Just $ fullSubject fInfo
                , _goguenRegistryEntry_name = Nothing
                , _goguenRegistryEntry_description = Nothing
                , _goguenRegistryEntry_preimage = Nothing
                , _goguenRegistryEntry_logo = Nothing
                }

    logoInfo <- case logoFname of
        Just fname -> do
            logoData <- BL.readFile fname
            let strictLogoData = BL.toStrict logoData
            let logoB64 = B64.encode strictLogoData
            let logoB64JSONText = T.pack $ withQuotes $ B8.unpack $ BL.fromStrict logoB64
            dieOnLeft "Verifying PNG" $ void $ decodePng strictLogoData -- verify validity, don't actually use decoding
            fmap Just $ dieOnLeft "Loading image data" $ do
                pv :: PropertyValue <- left T.unpack $ propertyValueFromString logoB64JSONText
                emptyAttested . WellKnown pv <$> Aeson.parseEither parseWellKnown pv
        Nothing ->
            pure Nothing

    let newRecord = combineRegistryEntries (newEntryInfo { _goguenRegistryEntry_logo = logoInfo }) record

    newRecordWithAttestations <- dieOnLeft "Adding attestation" $ case attestKey of
        Just k -> attestFields k attestProps newRecord
        Nothing -> pure newRecord

    newOwner <- dieOnLeft "Adding owner signature" $ case ownerKey of
        Just k -> Just <$> ownerSignature k newRecordWithAttestations
        Nothing -> pure $ do
            oldOwner' <- oldOwner
            case verifyRegistryOwnership (WithOwnership (Just oldOwner') newRecordWithAttestations) of
                Left _ -> Nothing
                Right _ -> pure oldOwner'

    let newRecordWithOwnership = WithOwnership newOwner newRecordWithAttestations
    let finalVerificationStatus = verifyEverything newRecordWithOwnership
    let outputString = show (serializeRegistryEntry newRecordWithOwnership) <> "\n"

    case _goguenRegistryEntry_preimage newRecordWithAttestations of
        Nothing -> pure ()
        Just preimage -> dieOnLeft "Checking preimage" $ do
            subject <- maybe (Left "Subject missing") Right $ _goguenRegistryEntry_subject newRecordWithAttestations
            verifyPreimage subject preimage

    case inputInfo of
        InputSourceFile fInfo -> do
            writeFile (draftFilename fInfo) outputString
            case _FileInfoDraftStatus fInfo of
                DraftStatusFinal -> do
                    dieOnLeft "Finalizing" finalVerificationStatus
                    renameFile (draftFilename fInfo) $ canonicalFilename fInfo
                    putStrLn $ canonicalFilename fInfo
                DraftStatusDraft -> do
                    putStrLn $ draftFilename fInfo
        InputSourceStdin -> do
            putStr outputString
        InputSourceCBORJson _ -> do
            subj <- dieOnLeft "Finding subject" $ case newRecordWithOwnership of
                WithOwnership _ (GoguenRegistryEntry (Just (Subject subj)) _ _ _ _) ->
                    pure subj
                _ ->
                    Left "No subject set"
            let fname = draftFilename $ FileInfo
                  { _FileInfoSubject = T.unpack subj
                  , _FileInfoTokenName = Nothing
                  , _FileInfoEntryOperation = EntryOperationInitialize
                  , _FileInfoDraftStatus = DraftStatusDraft
                  }
            writeFile fname outputString
            putStrLn fname
    exitSuccess
  where
    dieOnLeft :: String -> Either String a -> IO a
    dieOnLeft lbl eVal = case eVal of
        Left err  -> die $ T.pack $ lbl <> ": " <> err
        Right val -> pure val

    readKeyFile :: FilePath -> IO (SignKeyDSIGN Ed25519DSIGN)
    readKeyFile skFname = do
        lbs <- BL.readFile skFname
        dieOnLeft "Error reading key file" $ left show $
            decodeFullDecoder "Signing Key" decodeSignKeyDSIGN lbs

    parseJSON :: Either String Aeson.Value -> IO (WithOwnership Maybe PartialGoguenRegistryEntry)
    parseJSON registryJSON = dieOnLeft "Parse error" $ do
        json <- registryJSON
        Aeson.parseEither parseRegistryEntry json

keyGenerationParser :: OA.Parser KeyGenerationArguments
keyGenerationParser = KeyGenerationArguments <$>
    OA.strOption (OA.long "generate-key" <> OA.short 'K' <> OA.metavar "KEY")


handleKeyGeneration :: KeyGenerationArguments -> IO ()
handleKeyGeneration (KeyGenerationArguments fname) = do
    let pubKeyname = fname <> ".pub"
    let privKeyName = fname <> ".prv"
    let seedSize = seedSizeDSIGN (Proxy @Ed25519DSIGN)

    seed <- readSeedFromSystemEntropy seedSize

    let signKey = genKeyDSIGN seed
    let pubKey = deriveVerKeyDSIGN @Ed25519DSIGN signKey

    writeKeyFile pubKeyname $ encodeVerKeyDSIGN @Ed25519DSIGN pubKey
    writeKeyFile privKeyName $ encodeSignKeyDSIGN @Ed25519DSIGN signKey

writeKeyFile :: FilePath -> Encoding -> IO ()
writeKeyFile fname enc = do
    exists <- doesFileExist fname
    if exists
    then die $ T.pack $ "File already exists: " <> fname
    else BL.writeFile fname $ serializeEncoding enc

argumentParser :: Maybe String -> OA.Parser Arguments
argumentParser defaultSubject = asum
    [ ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject
    , ArgumentsKeyGeneration <$> keyGenerationParser
    ]
