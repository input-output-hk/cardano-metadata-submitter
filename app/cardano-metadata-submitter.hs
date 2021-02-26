{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Cardano.Prelude

import Cardano.Api
    ( AsType (AsPaymentExtendedKey, AsPaymentKey) )
import Cardano.CLI.Shelley.Key
    ( readSigningKeyFile )
import Cardano.CLI.Types
    ( SigningKeyFile (..) )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    , parseRegistryEntry
    , serializeRegistryEntry
    , verifyPolicy
    )
import Cardano.Metadata.Types
    ( Attested (..)
    , HashesForAttestation (..)
    , MakeAttestationSignature (..)
    , PropertyValue (..)
    , SomeSigningKey (..)
    , Subject (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , emptyAttested
    , evaluatePolicy
    , hashesForAttestation
    , prettyPolicy
    , propertyValueFromString
    , verifyAttested
    )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Control.Arrow
    ( left )
import Data.List
    ( isSuffixOf )
import Data.Time
    ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
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
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Prelude

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
  => (String -> Text)
  -> OA.Mod OA.OptionFields (WellKnown p)
  -> OA.Parser (WellKnown p)
wellKnownOption strTransform =
    OA.option wellKnownReader
  where
    wellKnownReader :: OA.ReadM (WellKnown p)
    wellKnownReader = OA.eitherReader $ \str -> do
        pv :: PropertyValue <- left T.unpack $ propertyValueFromString $ strTransform str
        WellKnown pv <$> Aeson.parseEither parseWellKnown pv

withQuotes :: String -> Text
withQuotes s = "\"" <> T.pack s <> "\""

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
        (Just (Attested sigA nA valA), Just (Attested sigB nB valB)) | raw valA == raw valB ->
            Just $ Attested (sigA ++ sigB) (max nA nB) valA
        (Just (Attested sigs nA val), Just (Attested _ nB _)) ->
            Just $ Attested sigs (max nA nB + 1) val
        _ ->
            a <|> b
    raw (WellKnown (PropertyValue r _) _) = r

attestFields
    :: SomeSigningKey
    -> [AttestationField]
    -> PartialGoguenRegistryEntry
    -> Either Text PartialGoguenRegistryEntry
attestFields (SomeSigningKey someSigningKey) props old = do
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
    attestField fld subj orig@(Attested att n wk) =
        if fld `elem` props
        then Attested attestations n wk
        else Attested att n wk
      where
        wkHash = extractAttestationHashes subj orig
        newAttestationSig = makeAttestationSignature someSigningKey wkHash
        attestations = newAttestationSig:att

extractAttestationHashes
    :: WellKnownProperty p
    => Subject
    -> Attested (WellKnown p)
    -> HashesForAttestation
extractAttestationHashes subj (Attested _ n (WellKnown raw structured)) =
    hashesForAttestation subj (wellKnownPropertyName (Identity structured)) raw n

verifyEverything
    :: SlotNo
    -> PartialGoguenRegistryEntry
    -> Either Text ()
verifyEverything atSlot record = do
    -- 1. Verify that mandatory fields are present
    subject <- verifyField _goguenRegistryEntry_subject
    policy  <- verifyField _goguenRegistryEntry_policy
    name    <- verifyField _goguenRegistryEntry_name
    desc    <- verifyField _goguenRegistryEntry_description

    -- 2. Policy should re-hash to first bytes of the subject
    verifyPolicy policy subject

    -- 3. Verify that all attestations have matching signatures
    let verifyLocalAttestations fieldName field = do
            let hashes = extractAttestationHashes subject field
            let (Attested attestations n _) = field
            left (const $ "attestation verification failed for: " <> fieldName) $ do
                verifyAttested $ Attested attestations n hashes
            let policyEvaluationFailed = unlines
                    [ "policy evaluation failed for: " <> fieldName
                    , "Policy is:"
                    , prettyPolicy policy
                    ]
            left (const policyEvaluationFailed) $
                evaluatePolicy policy atSlot attestations

    verifyLocalAttestations "name" name
    verifyLocalAttestations "description" desc

    forM_ (_goguenRegistryEntry_logo record) $ verifyLocalAttestations "logo"
    forM_ (_goguenRegistryEntry_url record) $ verifyLocalAttestations "url"
    forM_ (_goguenRegistryEntry_unit record) $ verifyLocalAttestations "unit"
    forM_ (_goguenRegistryEntry_ticker record) $ verifyLocalAttestations "ticker"
  where
    verifyField :: (PartialGoguenRegistryEntry -> Maybe a) -> Either Text a
    verifyField field = maybe (Left missingFields) Right (field record)

    missingFields :: Text
    missingFields = mconcat
        [ missingField "Missing field subject"
            _goguenRegistryEntry_subject
        , missingField "Missing field policy: Use -p to speciy"
            _goguenRegistryEntry_policy
        , missingField "Missing field name: Use -n to specify"
            _goguenRegistryEntry_name
        , missingField "Missing field description: Use -d to specify"
            _goguenRegistryEntry_description
        ]

    missingField :: Text -> (PartialGoguenRegistryEntry -> Maybe b) -> Text
    missingField str fld = case fld record of
        Just _  -> ""
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
            parseJSON (left T.pack json)
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
            let logoB64JSONText = withQuotes $ BL8.unpack $ BL.fromStrict logoB64
            fmap Just $ dieOnLeft "Loading image data" $ left T.pack $ do
                pv :: PropertyValue <- left T.unpack $ propertyValueFromString logoB64JSONText
                emptyAttested . WellKnown pv <$> Aeson.parseEither parseWellKnown pv
        Nothing ->
            pure Nothing

    let newRecord = combineRegistryEntries (newEntryInfo { _goguenRegistryEntry_logo = logoInfo }) record

    newRecordWithAttestations <- dieOnLeft "Adding attestation" $ case attestKey of
        Just k -> attestFields k attestProps newRecord
        Nothing -> pure newRecord

    -- FIXME: Allow users to specify a different start time and/or slot
    -- NOTE: Only useful for validating scripts which contains timelocks.
    slot <- getCurrentSlot mainnetShelleyStartTime mainnetShelleyStartSlot mainnetShelleySlotLength

    let finalVerificationStatus = verifyEverything slot newRecordWithAttestations
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
    dieOnLeft :: Text -> Either Text a -> IO a
    dieOnLeft lbl eVal = case eVal of
        Left err  -> die $ lbl <> ": " <> err
        Right val -> pure val

    readKeyFile :: FilePath -> IO SomeSigningKey
    readKeyFile skFname = do
        asNormalKey   <- fmap SomeSigningKey <$>
            readSigningKeyFile AsPaymentKey (SigningKeyFile skFname)
        asExtendedKey <- fmap SomeSigningKey <$>
            readSigningKeyFile AsPaymentExtendedKey (SigningKeyFile skFname)

        dieOnLeft "Error reading key file" $
            left show $ asNormalKey `orElse_` asExtendedKey

      where
        orElse_ a b = either (const b) Right a

    parseJSON :: Either Text Aeson.Value -> IO (PartialGoguenRegistryEntry)
    parseJSON registryJSON = dieOnLeft "Parse error" $ do
        json <- registryJSON
        left T.pack $ Aeson.parseEither parseRegistryEntry json

argumentParser :: Maybe Subject -> OA.Parser Arguments
argumentParser defaultSubject = asum
    [ ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject
    ]

getCurrentSlot :: UTCTime -> SlotNo -> NominalDiffTime -> IO SlotNo
getCurrentSlot startTime (SlotNo startSlot) slotLength = do
    now <- getCurrentTime
    let delta = nominalDiffTimeToSeconds $ now `diffUTCTime` startTime
    let slotsSinceShelley = fromIntegral $ delta `div` round slotLength
    pure $ SlotNo $ slotsSinceShelley + startSlot
  where
    nominalDiffTimeToSeconds :: NominalDiffTime -> Integer
    nominalDiffTimeToSeconds = round

mainnetShelleyStartTime :: UTCTime
mainnetShelleyStartTime = Prelude.read "2020-07-29 21:44:51 UTC"

mainnetShelleyStartSlot :: SlotNo
mainnetShelleyStartSlot = SlotNo 4492800

mainnetShelleySlotLength :: NominalDiffTime
mainnetShelleySlotLength = 1
