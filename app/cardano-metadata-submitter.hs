{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Cardano.Prelude

import Cardano.Binary
    ( decodeFullDecoder )
import Cardano.Crypto.DSIGN
    ( Ed25519DSIGN, SignKeyDSIGN, decodeSignKeyDSIGN )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    , parseRegistryEntry
    , serializeRegistryEntry
    )
import Cardano.Metadata.Types
    ( Attested (..)
    , HashesForAttestation (..)
    , Logo (..)
    , PropertyValue (..)
    , Subject (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , emptyAttested
    , hashesForAttestation
    , makeAttestationSignature
    , propertyValueFromString
    , verifyAttested
    )
import Codec.Picture.Png
    ( decodePng )
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import qualified Options.Applicative as OA

main :: IO ()
main = do
    defaultSubject <- fmap (Subject . T.pack) <$> lookupEnv "METADATA_SUBJECT"
    args <- OA.execParser $ OA.info (argumentParser defaultSubject <**> OA.helper) mempty
    case args of
        ArgumentsEntryUpdate eua  -> handleEntryUpdateArguments eua

data DraftStatus
    = DraftStatusDraft
    | DraftStatusFinal
    deriving Show

data EntryOperation
    = EntryOperationInitialize
    | EntryOperationRevise
    deriving Show

data AttestationField
    = AttestationFieldName
    | AttestationFieldDescription
    | AttestationFieldLogo
    | AttestationFieldUrl
    | AttestationFieldUnit
    | AttestationFieldTicker
    deriving (Show, Eq, Ord)

data FileInfo = FileInfo
    { _FileInfoSubject :: Subject
    , _FileInfoEntryOperation :: EntryOperation
    , _FileInfoDraftStatus :: DraftStatus
    }
  deriving Show

canonicalFilename :: FileInfo -> String
canonicalFilename = (<> jsonSuffix) . T.unpack . unSubject . _FileInfoSubject

jsonSuffix, draftSuffix, jsonDraftSuffix :: String
jsonSuffix = ".json"
draftSuffix = ".draft"
jsonDraftSuffix = jsonSuffix <> draftSuffix

draftFilename :: FileInfo -> String
draftFilename fi = canonicalFilename fi <> draftSuffix

data EntryUpdateArguments = EntryUpdateArguments
    { _EntryUpdateArgumentsFileInfo :: FileInfo
    , _EntryUpdateArgumentsAttestationKeyFilename :: Maybe String
    , _EntryUpdateArgumentsAttestationFields :: [AttestationField]
    , _EntryUpdateArgumentsRegistryEntry :: PartialGoguenRegistryEntry
    , _EntryUpdateLogoFilename :: Maybe String
    }
    deriving Show

data Arguments
    = ArgumentsEntryUpdate EntryUpdateArguments
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

entryUpdateArgumentParser :: Maybe Subject -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments
    <$> fileInfoArgumentParser
    <*> optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE"))
    <*> attestationFieldNamesParser
    <*> goguenRegistryEntryParser
    <*> logoFilenameParser
  where
    attestationFieldNamesParser :: OA.Parser [AttestationField]
    attestationFieldNamesParser = asum
        [ OA.flag' [AttestationFieldName] $ OA.long "attest-name" <> OA.short 'N'
        , OA.flag' [AttestationFieldDescription] $ OA.long "attest-description" <> OA.short 'D'
        , OA.flag' [AttestationFieldLogo] $ OA.long "attest-logo" <> OA.short 'L'
        , OA.flag' [AttestationFieldUrl] $ OA.long "attest-url" <> OA.short 'H'
        , OA.flag' [AttestationFieldUnit] $ OA.long "attest-unit" <> OA.short 'U'
        , OA.flag' [AttestationFieldTicker] $ OA.long "attest-ticker" <> OA.short 'T'
        , pure
            [ AttestationFieldName
            , AttestationFieldDescription
            , AttestationFieldLogo
            , AttestationFieldUrl
            , AttestationFieldUnit
            , AttestationFieldTicker
            ]
       ]

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo
        <$> (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT") <|> defaultSubjectParser)
        <*> OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i')
        <*> OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    defaultSubjectParser =
        maybe empty pure defaultSubject

    trimSubject :: String -> Subject
    trimSubject subj
        | jsonSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonSuffix) subj
        | jsonDraftSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonDraftSuffix) subj
        | otherwise =
            Subject $ T.pack subj

    logoFilenameParser :: OA.Parser (Maybe String)
    logoFilenameParser = optional $ OA.strOption (OA.long "logo" <> OA.short 'l' <> OA.metavar "LOGO.png")

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry Nothing
        <$> optional (_wellKnown_structured <$> wellKnownOption withQuotes (OA.long "policy" <> OA.short 'p' <> OA.metavar "POLICY"))
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
        <*> pure Nothing
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "url" <> OA.short 'h' <> OA.metavar "URL"))
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "unit" <> OA.short 'u' <> OA.metavar "UNIT"))
        <*> optional (emptyAttested <$> wellKnownOption withQuotes (OA.long "ticker" <> OA.short 't' <> OA.metavar "TICKER"))

combineRegistryEntries
    :: GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
    { _goguenRegistryEntry_subject =
        _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
    , _goguenRegistryEntry_policy =
        _goguenRegistryEntry_policy new <|> _goguenRegistryEntry_policy old
    , _goguenRegistryEntry_name =
        _goguenRegistryEntry_name new `combineAttestedEntry` _goguenRegistryEntry_name old
    , _goguenRegistryEntry_description =
        _goguenRegistryEntry_description new `combineAttestedEntry` _goguenRegistryEntry_description old
    , _goguenRegistryEntry_logo =
        _goguenRegistryEntry_logo new `combineAttestedEntry` _goguenRegistryEntry_logo old
    , _goguenRegistryEntry_url =
        _goguenRegistryEntry_url new `combineAttestedEntry` _goguenRegistryEntry_url old
    , _goguenRegistryEntry_unit =
        _goguenRegistryEntry_unit new `combineAttestedEntry` _goguenRegistryEntry_unit old
    , _goguenRegistryEntry_ticker =
        _goguenRegistryEntry_ticker new `combineAttestedEntry` _goguenRegistryEntry_ticker old
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
        , _goguenRegistryEntry_url =
            attestField AttestationFieldUrl subj <$> _goguenRegistryEntry_url old
        , _goguenRegistryEntry_unit =
            attestField AttestationFieldUnit subj <$> _goguenRegistryEntry_unit old
        , _goguenRegistryEntry_ticker =
            attestField AttestationFieldTicker subj <$> _goguenRegistryEntry_ticker old
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

verifyEverything
    :: PartialGoguenRegistryEntry
    -> Either String ()
verifyEverything record = do
    -- these fields are mandatory
    subj <- verifyField _goguenRegistryEntry_subject
    name <- verifyField _goguenRegistryEntry_name
    desc <- verifyField _goguenRegistryEntry_description

    let verifyLocalAttestations fieldName field = do
            let hashes = extractAttestationHashes subj field
            let (Attested attestations _) = field
            left (const $ fieldName <> " attestation verification failed") $
                verifyAttested $ Attested attestations hashes

    verifyLocalAttestations "Name" name
    verifyLocalAttestations "Description" desc

    forM_ (_goguenRegistryEntry_logo record) $ \logo -> do
        verifyLocalAttestations "Logo" logo
        -- FIXME: Move to parser
        let (Attested _ (WellKnown _ (Logo logoData))) = logo
        void $ left ("Verifying PNG: " <>) $ decodePng $ BL.toStrict logoData
  where
    verifyField :: (PartialGoguenRegistryEntry -> Maybe a) -> Either String a
    verifyField field = maybe (Left missingFields) Right (field record)

    missingFields = concat
        [ missingField "Missing field subject" _goguenRegistryEntry_subject
        , missingField "Missing field name: Use -n to specify" _goguenRegistryEntry_name
        , missingField "Missing field description: Use -d to specify" _goguenRegistryEntry_description
        ]

    missingField str fld = case fld record of
        Just _ -> ""
        Nothing -> "\n" <> str

handleEntryUpdateArguments :: EntryUpdateArguments -> IO ()
handleEntryUpdateArguments (EntryUpdateArguments fInfo attestKeyFile attestProps newEntryInfo logoFname) = do
    attestKey <- mapM readKeyFile attestKeyFile

    record <- case _FileInfoEntryOperation fInfo of
        EntryOperationRevise -> do
            let dfn = draftFilename fInfo
            exists <- doesFileExist $ draftFilename fInfo
            let readFn = if exists then dfn else canonicalFilename fInfo
            json <- Aeson.eitherDecodeFileStrict readFn
            parseJSON json
        EntryOperationInitialize -> pure $ GoguenRegistryEntry
            { _goguenRegistryEntry_subject = Just (_FileInfoSubject fInfo)
            , _goguenRegistryEntry_policy = Nothing
            , _goguenRegistryEntry_name = Nothing
            , _goguenRegistryEntry_description = Nothing
            , _goguenRegistryEntry_logo = Nothing
            , _goguenRegistryEntry_url = Nothing
            , _goguenRegistryEntry_unit = Nothing
            , _goguenRegistryEntry_ticker = Nothing
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

    let finalVerificationStatus = verifyEverything newRecordWithAttestations
    let outputString = show (serializeRegistryEntry newRecordWithAttestations) <> "\n"

    writeFile (draftFilename fInfo) outputString
    case _FileInfoDraftStatus fInfo of
        DraftStatusFinal -> do
            dieOnLeft "Finalizing" finalVerificationStatus
            renameFile (draftFilename fInfo) $ canonicalFilename fInfo
            putStrLn $ canonicalFilename fInfo
        DraftStatusDraft -> do
            putStrLn $ draftFilename fInfo

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

    parseJSON :: Either String Aeson.Value -> IO (PartialGoguenRegistryEntry)
    parseJSON registryJSON = dieOnLeft "Parse error" $ do
        json <- registryJSON
        Aeson.parseEither parseRegistryEntry json

argumentParser :: Maybe Subject -> OA.Parser Arguments
argumentParser defaultSubject = asum
    [ ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject
    ]
