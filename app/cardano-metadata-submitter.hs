{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
    , validateEntry
    )
import Cardano.Metadata.Types
    ( Attested (..)
    , MakeAttestationSignature (..)
    , SomeSigningKey (..)
    , Subject (..)
    , WellKnownProperty (..)
    , emptyAttested
    , hashesForAttestation
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
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
    , _EntryUpdatePolicyFilenameOrCBOR :: Maybe String
    }
    deriving Show

data Arguments
    = ArgumentsEntryUpdate EntryUpdateArguments
    deriving Show

wellKnownOption
    :: forall p. WellKnownProperty p
    => OA.Mod OA.OptionFields p
    -> OA.Parser p
wellKnownOption =
    OA.option wellKnownReader
  where
    wellKnownReader :: OA.ReadM p
    wellKnownReader = OA.eitherReader $
        Aeson.parseEither parseWellKnown . Aeson.toJSON

entryUpdateArgumentParser :: Maybe Subject -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments
    <$> fileInfoArgumentParser
    <*> optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE"))
    <*> attestationFieldNamesParser
    <*> goguenRegistryEntryParser
    <*> logoFilenameParser
    <*> policyParser
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

    policyParser :: OA.Parser (Maybe String)
    policyParser = optional (OA.strOption (OA.long "policy" <> OA.short 'p' <> OA.metavar "POLICY"))

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry Nothing Nothing
        <$> optional (emptyAttested <$> wellKnownOption (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
        <*> pure Nothing -- logo
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "url" <> OA.short 'h' <> OA.metavar "URL"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "unit" <> OA.short 'u' <> OA.metavar "UNIT"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "ticker" <> OA.short 't' <> OA.metavar "TICKER"))

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
        (Just (Attested sigA nA valA), Just (Attested sigB nB valB)) | valA == valB ->
            Just $ Attested (sigA ++ sigB) (max nA nB) valA
        (Just (Attested sigs nA val), Just (Attested _ nB _)) ->
            Just $ Attested sigs (max nA nB + 1) val
        _ ->
            a <|> b

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
        -> Attested p
        -> Attested p
    attestField fld subj (Attested att n wk) =
        if fld `elem` props
        then Attested attestations n wk
        else Attested att n wk
      where
        wkHash = hashesForAttestation subj wk n
        newAttestationSig = makeAttestationSignature someSigningKey wkHash
        attestations = newAttestationSig:att

handleEntryUpdateArguments :: EntryUpdateArguments -> IO ()
handleEntryUpdateArguments (EntryUpdateArguments fInfo keyfile props newEntryInfo logoM policyM) = do
    attestKey <- mapM readKeyFile keyfile

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

    policy <- case policyM of
        Just filenameOrCBOR -> do
            result <- doesFileExist filenameOrCBOR >>= \case
                True  -> do
                    json <- Aeson.eitherDecodeFileStrict filenameOrCBOR
                    pure $ Aeson.parseEither parseWellKnown =<< json
                False ->
                    pure $ Aeson.parseEither parseWellKnown (Aeson.toJSON filenameOrCBOR)
            fmap Just $ dieOnLeft "Loading policy" $ left T.pack result
        Nothing ->
            pure Nothing

    logo <- case logoM of
        Just fname -> do
            logo <- BS.readFile fname
            let logoAsJSON = Aeson.toJSON $ B8.unpack $ B64.encode logo
            fmap Just $ dieOnLeft "Loading image data" $ left T.pack $
                emptyAttested <$> Aeson.parseEither parseWellKnown logoAsJSON
        Nothing ->
            pure Nothing

    let newRecord = combineRegistryEntries (newEntryInfo
            { _goguenRegistryEntry_logo = logo
            , _goguenRegistryEntry_policy = policy
            }) record

    newRecordWithAttestations <- dieOnLeft "Adding attestation" $ case attestKey of
        Just k -> attestFields k props newRecord
        Nothing -> pure newRecord

    let finalVerificationStatus = validateEntry newRecordWithAttestations

    BL8.writeFile (draftFilename fInfo) (Aeson.encodePretty newRecordWithAttestations)
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
