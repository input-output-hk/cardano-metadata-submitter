{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Cardano.Api
    ( serialiseToRawBytesHex )
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
    , PropertyValue (..)
    , SequenceNumber (..)
    , Subject (..)
    , Ticker (..)
    , Unit (..)
    , Url (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , hashPolicy
    , parseUnit
    , propertyValueFromString
    , propertyValueToString
    )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( (.:), (.:?) )

import qualified AesonHelpers
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as B8
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

type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

parseRegistryEntry
    :: Aeson.Value
    -> Aeson.Parser PartialGoguenRegistryEntry
parseRegistryEntry = Aeson.withObject "GoguenRegistryEntry" $ \o -> do
    subject <- o .:? "subject"

    policyRaw <- o .:? unProperty (wellKnownPropertyName $ Proxy @Policy)
    policy <- fmap _wellKnown_structured <$> mapM parseWellKnownGoguen policyRaw

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
    unitAnn   <- mapM (parseWithAttestation parseWellKnownUnit) unitField
    tickerAnn <- mapM (parseWithAttestation parseWellKnownGoguen) tickerField

    pure $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Subject <$> subject
        , _goguenRegistryEntry_policy  = policy
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

parseWellKnownUnit
    :: Aeson.Value
    -> Aeson.Parser (WellKnown Unit)
parseWellKnownUnit v =
    parseUnit v <&> \u ->
        let
            rawUnit = mconcat
                [ "\""
                , show (unitDecimals u)
                , ","
                , show (unitName u)
                , "\""
                ]
        in
            WellKnown (PropertyValue rawUnit v) u

parseWithAttestation
    :: (Aeson.Value -> Aeson.Parser a)
    -> Aeson.Value
    -> Aeson.Parser (Attested a)
parseWithAttestation parseValue = Aeson.withObject "property with attestation" $ \o -> do
    value <- parseValue =<< o .: "value"
    attestations <- (o .: "signatures" >>=) $ Aeson.withArray "Annotated Signatures" $
        fmap toList . mapM (Aeson.withObject "Attestation" (parseAnnotatedSignature AttestationSignature))
    sequenceNumber <- SequenceNumber <$> o .: "sequence_number"
    pure $ Attested
        { _attested_signatures = attestations
        , _attested_property = value
        , _attested_sequence_number = sequenceNumber
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

verifyPolicy
    :: Policy
    -> Subject
    -> Either Text ()
verifyPolicy policy (Subject subject) = do
    let policyId = T.pack . B8.unpack . serialiseToRawBytesHex . hashPolicy $ policy
    unless (policyId `T.isPrefixOf` subject) $ Left $ unlines
        [ "The policy should re-hash to the first 28 bytes of the subject."
        , "Expected: " <> T.take 56 subject
        , "Got: " <> policyId
        ]

-- FIXME: Replace this with generics...
serializeRegistryEntry
    :: PartialGoguenRegistryEntry
    -> PP.Doc ann
serializeRegistryEntry entry = encloseObj $ catMaybes
    [ flip fmap (_goguenRegistryEntry_subject entry) $ \subject -> objField "subject" $
        PP.dquotes $ PP.pretty $ unSubject subject
    , flip fmap (_goguenRegistryEntry_policy entry) $ \policy -> objField "policy" $
        PP.dquotes $ PP.pretty $ rawPolicy policy
    , flip fmap (_goguenRegistryEntry_name entry) $ \name -> objField "name" $
        attested prettyWellKnown name
    , flip fmap (_goguenRegistryEntry_description entry) $ \description -> objField "description" $
        attested prettyWellKnown description
    , flip fmap (_goguenRegistryEntry_logo entry) $ \logo -> objField "logo" $
        attested prettyWellKnown logo
    , flip fmap (_goguenRegistryEntry_url entry) $ \url -> objField "url" $
        attested prettyWellKnown url
    , flip fmap (_goguenRegistryEntry_unit entry) $ \unit_ -> objField "unit" $
        attested prettyUnit unit_
    , flip fmap (_goguenRegistryEntry_ticker entry) $ \ticker -> objField "ticker" $
        attested prettyWellKnown ticker
    ]
 where
    prettyBytes = PP.dquotes . PP.pretty . T.encodeHex
    prettyWellKnown = PP.pretty . propertyValueToString . _wellKnown_raw
    prettyUnit (WellKnown _ u) = encloseObj
        [ objField "name" (PP.dquotes $ PP.pretty $ unitName u)
        , objField "decimals" (PP.pretty $ unitDecimals u)
        ]
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
        , objField "sequence_number" (PP.pretty (toInteger $ _attested_sequence_number a))
        , objField "signatures" $ encloseList $
            attestation <$> _attested_signatures a
        ]
    attestation s = encloseObj
        [ objField "publicKey" $ prettyBytes $ rawSerialiseVerKeyDSIGN $
            _attestationSignature_publicKey s
        , objField "signature" $ prettyBytes $ rawSerialiseSigDSIGN $
            _attestationSignature_signature s
        ]
