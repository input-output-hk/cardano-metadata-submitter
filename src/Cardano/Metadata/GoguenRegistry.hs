{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Cardano.Crypto.DSIGN.Class
    ( DSIGNAlgorithm
    , SigDSIGN
    , VerKeyDSIGN
    , rawDeserialiseSigDSIGN
    , rawDeserialiseVerKeyDSIGN
    , rawSerialiseSigDSIGN
    , rawSerialiseVerKeyDSIGN
    )
import Cardano.Metadata.Types
    ( AttestationSignature (..)
    , Attested (..)
    , Description (..)
    , Logo (..)
    , Logo (..)
    , Name (..)
    , Policy (..)
    , Property (..)
    , Subject (..)
    , Ticker (..)
    , Unit (..)
    , Url (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , hashesForAttestation
    , propertyValueFromString
    , propertyValueToString
    , verifyAttested
    , withWellKnown
    )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( (.:), (.:?) )
import Prelude
    ( error )

import qualified AesonHelpers
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Prettyprinter as PP
import qualified Text.Hex as T

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
data GoguenRegistryEntry f = GoguenRegistryEntry
    { _goguenRegistryEntry_subject :: f Subject
    , _goguenRegistryEntry_policy :: f Policy
    -- ^ Neither the policy nor The subject are attested.
    --   Attestations attest to the relationship between the subject and the
    --   field and thus already include the subject in the hash. The policy
    --   itself is used to verify attestations.

    , _goguenRegistryEntry_name :: f (Attested (WellKnown Name))
    , _goguenRegistryEntry_description :: f (Attested (WellKnown Description))

    -- Fields below are optional.
    , _goguenRegistryEntry_logo :: f (Attested (WellKnown Logo))
    , _goguenRegistryEntry_url :: f (Attested (WellKnown Url))
    , _goguenRegistryEntry_unit :: f (Attested (WellKnown Unit))
    , _goguenRegistryEntry_ticker :: f (Attested (WellKnown Ticker))
    }

deriving instance
    ( Show (f Subject)
    , Show (f Policy)
    , Show (f (Attested (WellKnown Name)))
    , Show (f (Attested (WellKnown Description)))
    , Show (f (Attested (WellKnown Logo)))
    , Show (f (Attested (WellKnown Url)))
    , Show (f (Attested (WellKnown Unit)))
    , Show (f (Attested (WellKnown Ticker)))
    ) => Show (GoguenRegistryEntry f)

type CompleteGoguenRegistryEntry = GoguenRegistryEntry Identity
type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

completeToPartialRegistryEntry
    :: CompleteGoguenRegistryEntry
    -> PartialGoguenRegistryEntry
completeToPartialRegistryEntry e = GoguenRegistryEntry
    { _goguenRegistryEntry_subject =
        Just $ runIdentity $ _goguenRegistryEntry_subject e
    , _goguenRegistryEntry_policy =
        Just $ runIdentity $ _goguenRegistryEntry_policy e
    , _goguenRegistryEntry_name =
        Just $ runIdentity $ _goguenRegistryEntry_name e
    , _goguenRegistryEntry_description =
        Just $ runIdentity $ _goguenRegistryEntry_description e
    , _goguenRegistryEntry_logo =
        Just $ runIdentity $ _goguenRegistryEntry_logo e
    , _goguenRegistryEntry_url =
        Just $ runIdentity $ _goguenRegistryEntry_url e
    , _goguenRegistryEntry_unit =
        Just $ runIdentity $ _goguenRegistryEntry_unit e
    , _goguenRegistryEntry_ticker =
        Just $ runIdentity $ _goguenRegistryEntry_ticker e
    }

partialToCompleteRegistryEntry
    :: PartialGoguenRegistryEntry
    -> Maybe CompleteGoguenRegistryEntry
partialToCompleteRegistryEntry e = GoguenRegistryEntry
    <$> fmap Identity (_goguenRegistryEntry_subject e)
    <*> fmap Identity (_goguenRegistryEntry_policy e)
    <*> fmap Identity (_goguenRegistryEntry_name e)
    <*> fmap Identity (_goguenRegistryEntry_description e)
    <*> fmap Identity (_goguenRegistryEntry_logo e)
    <*> fmap Identity (_goguenRegistryEntry_url e)
    <*> fmap Identity (_goguenRegistryEntry_unit e)
    <*> fmap Identity (_goguenRegistryEntry_ticker e)

parseRegistryEntry
    :: Aeson.Value
    -> Aeson.Parser PartialGoguenRegistryEntry
parseRegistryEntry = Aeson.withObject "GoguenRegistryEntry" $ \o -> do
    -- FIXME: Have proper parser for subject and policy. We expect both to be
    -- hex-encoded strings.
    subject <- o .:? "subject"
    policy  <- o .:? "policy"

    nameField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Name)
    descField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Description)
    logoField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Logo)
    urlField    <- o .:? unProperty (wellKnownPropertyName $ Proxy @Url)
    unitField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Unit)
    tickerField <- o .:? unProperty (wellKnownPropertyName $ Proxy @Ticker)

    nameAnn   <- mapM (parseWithAttestation parseWellKnownGoguen) nameField
    descAnn   <- mapM (parseWithAttestation parseWellKnownGoguen) descField
    logoAnn   <- mapM (parseWithAttestation parseWellKnownGoguen) logoField
    urlAnn    <- mapM (parseWithAttestation parseWellKnownGoguen) urlField
    unitAnn   <- mapM (parseWithAttestation parseWellKnownGoguen) unitField
    tickerAnn <- mapM (parseWithAttestation parseWellKnownGoguen) tickerField

    pure $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Subject <$> subject
        , _goguenRegistryEntry_policy  = Policy <$> policy
        , _goguenRegistryEntry_name = nameAnn
        , _goguenRegistryEntry_description = descAnn
        , _goguenRegistryEntry_logo = logoAnn
        , _goguenRegistryEntry_url = urlAnn
        , _goguenRegistryEntry_unit = unitAnn
        , _goguenRegistryEntry_ticker = tickerAnn
        }

-- | The registry encodes "name" and "description" differently from the CIP.
-- in particular `{ "value": "Foo" }` instead of `{ "value" : "\"Foo\"" }`.
parseWellKnownGoguen
    :: WellKnownProperty p
    => Aeson.Value
    -> Aeson.Parser (WellKnown p)
parseWellKnownGoguen =
    let
        parse t =
            case propertyValueFromString ("\"" <> t <> "\"") of
                Left err -> fail (T.unpack err)
                Right v -> WellKnown v <$> parseWellKnown v
    in
        Aeson.withText "property value" parse

parseWithAttestation
    :: (Aeson.Value -> Aeson.Parser a)
    -> Aeson.Value
    -> Aeson.Parser (Attested a)
parseWithAttestation parseValue = Aeson.withObject "property with attestation" $ \o -> do
    value <- parseValue =<< o .: "value"
    attestations <- (o .: "anSignatures" >>=) $ Aeson.withArray "Annotated Signatures" $
        fmap toList . mapM (Aeson.withObject "Attestation" (parseAnnotatedSignature AttestationSignature))
    pure $ Attested
        { _attested_signatures = attestations
        , _attested_property = value
        }

parseAnnotatedSignature
    :: DSIGNAlgorithm v
    => (VerKeyDSIGN v -> SigDSIGN v -> x)
    -> Aeson.Object
    -> Aeson.Parser x
parseAnnotatedSignature f o = do
    publicKeyField <- o .: "publicKey"
    signatureField <- o .: "signature"
    publicKey <- flip (Aeson.withText "publicKey") publicKeyField $ \t ->
        maybe (fail $ T.unpack $ "Couldn't parse verification key: " <> t) pure $
            rawDeserialiseVerKeyDSIGN =<< T.decodeHex t
    signature <- flip (Aeson.withText "signature") signatureField $ \t ->
        maybe (fail $ T.unpack $ "Couldn't parse signature " <> t) pure $
            rawDeserialiseSigDSIGN =<< T.decodeHex t
    AesonHelpers.noOtherFields "annotated signature" o ["publicKey", "signature"]
    pure $ f publicKey signature

-- | The subject is made of the concatenation of the policy id and asset name. The
-- asset name is potentially empty. Therefore, the first 28 bytes of the subject
-- are the policy id.
verifyPolicy
    :: (Monoid s, IsString s)
    => Subject
    -> Policy
    -> Either s ()
verifyPolicy _subject _policy =
    error "FIXME: verifyPolicy"
--    case T.decodeHex subjectHex of
--        Nothing ->
--            Left "Subject is not a byte string"
--        Just subject -> do
--            hasher <- case fromHashFnIdentifier (_preimage_hashFn preimage) of
--                Right (Some h) -> Right $ case h of
--                    (SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash h) ->
--                        hashToBytes . hashWith @h id
--                    (SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash h) ->
--                        hashToBytes . hashWith @h id
--                    (SupportedPreimageHash_SHA256 :: SupportedPreimageHash h) ->
--                        hashToBytes . hashWith @h id
--                Left () -> Left $ mconcat
--                    [ "Hash function not supported. Supported functions: "
--                    , "sha256, blake2b-256, blake2b-224"
--                    ]
--            case T.decodeHex (_preimage_preimage preimage) of
--                Nothing ->
--                    Left "Preimage is not a byte string"
--                Just preimageBytes -> do
--                    if hasher preimageBytes == subject
--                    then pure ()
--                    else Left "Hashed preimage does not equal subject"
--
--hashPreimage
--  :: (Monoid s, IsString s)
--  => Preimage
--  -> Either s Subject
--hashPreimage preimage = do
--    hasher <- case fromHashFnIdentifier (_preimage_hashFn preimage) of
--        Right (Some h) -> Right $ case h of
--            (SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash h) ->
--                hashToBytes . hashWith @h id
--            (SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash h) ->
--                hashToBytes . hashWith @h id
--            (SupportedPreimageHash_SHA256 :: SupportedPreimageHash h) ->
--                hashToBytes . hashWith @h id
--        Left () -> Left $ mconcat
--            [ "Hash function not supported. Supported functions: "
--            , "sha256, blake2b-256, blake2b-224"
--            ]
--    case T.decodeHex (_preimage_preimage preimage) of
--        Nothing -> Left "Preimage is not a byte string"
--        Just preimageBytes -> pure $ Subject $ T.encodeHex $ hasher preimageBytes

verifyAttestations
    :: CompleteGoguenRegistryEntry
    -> Either [AttestationSignature] ()
verifyAttestations entry = do
    let Identity subject = _goguenRegistryEntry_subject entry
    let Identity name = _goguenRegistryEntry_name entry
    let Identity description = _goguenRegistryEntry_description entry
    _ <- verifyAttested $ fmap (`withWellKnown` (hashesForAttestation subject)) name
    _ <- verifyAttested $ fmap (`withWellKnown` (hashesForAttestation subject)) description
    pure ()

-- FIXME: Replace this with generics...
serializeRegistryEntry
    :: PartialGoguenRegistryEntry
    -> PP.Doc ann
serializeRegistryEntry entry = encloseObj $ catMaybes
    [ flip fmap (_goguenRegistryEntry_subject entry) $ \subject -> objField "subject" $
        PP.dquotes $ PP.pretty $ unSubject subject
    , flip fmap (_goguenRegistryEntry_policy entry) $ \policy -> objField "policy" $
        PP.dquotes $ PP.pretty $ unPolicy policy
    , flip fmap (_goguenRegistryEntry_name entry) $ \name -> objField "name" $
        attested prettyWellKnown name
    , flip fmap (_goguenRegistryEntry_description entry) $ \description -> objField "description" $
        attested prettyWellKnown description
    , flip fmap (_goguenRegistryEntry_logo entry) $ \logo -> objField "logo" $
        attested prettyWellKnown logo
    , flip fmap (_goguenRegistryEntry_url entry) $ \url -> objField "url" $
        attested prettyWellKnown url
    , flip fmap (_goguenRegistryEntry_unit entry) $ \unit_ -> objField "unit" $
        attested prettyWellKnown unit_
    , flip fmap (_goguenRegistryEntry_ticker entry) $ \ticker -> objField "ticker" $
        attested prettyWellKnown ticker
    ]
 where
    prettyBytes = PP.dquotes . PP.pretty . T.encodeHex
    prettyWellKnown = PP.pretty . propertyValueToString . _wellKnown_raw
    encloseObj fs = PP.vcat
        [ PP.nest 4 $ PP.vcat $ PP.lbrace : PP.punctuate PP.comma fs
        , PP.rbrace
        ]
    encloseList xs = PP.vcat
      [ PP.nest 4 $ PP.vcat $ PP.lbracket : PP.punctuate PP.comma xs
      , PP.rbracket
      ]
    objField fieldName contents = PP.hsep
      [ PP.dquotes fieldName <> PP.colon
      , contents
      ]
    attested prettyValue a = encloseObj
        [ objField "value" (prettyValue (_attested_property a))
        , objField "anSignatures" $ encloseList $
            fmap attestation $ _attested_signatures a
        ]
    attestation s = encloseObj
        [ objField "publicKey" $ prettyBytes $ rawSerialiseVerKeyDSIGN $
            _attestationSignature_publicKey s
        , objField "signature" $ prettyBytes $ rawSerialiseSigDSIGN $
            _attestationSignature_signature s
        ]
