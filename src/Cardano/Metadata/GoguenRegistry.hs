{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Crypto.Hash
    ( Blake2b_224
    , Blake2b_256
    , HashAlgorithm (..)
    , SHA256
    , hashToBytes
    , hashWith
    )
import Cardano.Metadata.Types
    ( AttestationSignature (..)
    , Attested (..)
    , Description (..)
    , HashesForOwnership (..)
    , Logo (..)
    , Name (..)
    , OwnershipSignature (..)
    , Preimage (..)
    , Property (..)
    , PropertyValue (..)
    , Subject (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , WithOwnership (..)
    , hashProperty
    , hashPropertyValue
    , hashSubject
    , hashesForAttestation
    , propertyValueFromString
    , propertyValueToString
    , verifyAttested
    , verifyOwnership
    , withWellKnown
    )
import Control.Arrow
    ( left )
import Control.Category
    ( id )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( (.:), (.:?) )
import Data.Some
    ( Some (..) )
import Data.Tagged
    ( Tagged (..) )
import Prelude
    ( String )

import qualified AesonHelpers
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Prettyprinter as PP
import qualified Text.Hex as T

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
data GoguenRegistryEntry f = GoguenRegistryEntry
    { _goguenRegistryEntry_subject :: f Subject
    -- ^ The subject is not attested because it would not make sense to:
    --   attestations attest to the relationship between the
    --   subject and the field and thus already include the subject in the hash.
    , _goguenRegistryEntry_name :: f (Attested (WellKnown Name))
    , _goguenRegistryEntry_description :: f (Attested (WellKnown Description))
    , _goguenRegistryEntry_logo :: f (Attested (WellKnown Logo)) -- This is optional, others are mandatory
                                                                 -- TODO: Maybe model that better
    , _goguenRegistryEntry_preimage :: f Preimage
    -- ^ The preimage is not attested because it is directly verifiable.
    }

deriving instance
    ( Show (f Subject)
    , Show (f (Attested (WellKnown Name)))
    , Show (f (Attested (WellKnown Description)))
    , Show (f Preimage)
    , Show (f (Attested (WellKnown Logo)))
    ) => Show (GoguenRegistryEntry f)

type CompleteGoguenRegistryEntry = GoguenRegistryEntry Identity
type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

completeToPartialRegistryEntry
    :: CompleteGoguenRegistryEntry
    -> PartialGoguenRegistryEntry
completeToPartialRegistryEntry e = GoguenRegistryEntry
    { _goguenRegistryEntry_subject = Just $ runIdentity $ _goguenRegistryEntry_subject e
    , _goguenRegistryEntry_name = Just $ runIdentity $ _goguenRegistryEntry_name e
    , _goguenRegistryEntry_description = Just $ runIdentity $ _goguenRegistryEntry_description e
    , _goguenRegistryEntry_logo = Just $ runIdentity $ _goguenRegistryEntry_logo e
    , _goguenRegistryEntry_preimage = Just $ runIdentity $ _goguenRegistryEntry_preimage e
    }

partialToCompleteRegistryEntry
    :: PartialGoguenRegistryEntry
    -> Maybe CompleteGoguenRegistryEntry
partialToCompleteRegistryEntry e = GoguenRegistryEntry
    <$> fmap Identity (_goguenRegistryEntry_subject e)
    <*> fmap Identity (_goguenRegistryEntry_name e)
    <*> fmap Identity (_goguenRegistryEntry_description e)
    <*> fmap Identity (_goguenRegistryEntry_logo e)
    <*> fmap Identity (_goguenRegistryEntry_preimage e)

data SupportedPreimageHash h where
    SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash Blake2b_256
    SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash Blake2b_224
    SupportedPreimageHash_SHA256 :: SupportedPreimageHash SHA256

toHashFnIdentifier
    :: SupportedPreimageHash h
    -> Text
toHashFnIdentifier = \case
    SupportedPreimageHash_Blake2b_256 -> "blake2b-256"
    SupportedPreimageHash_Blake2b_224 -> "blake2b-224"
    SupportedPreimageHash_SHA256 -> "sha256"

fromHashFnIdentifier
    :: Text
    -> Either () (Some SupportedPreimageHash)
fromHashFnIdentifier fn
    | fn == "blake2b-256" = Right $ Some
        SupportedPreimageHash_Blake2b_256
    | fn == "blake2b-224" = Right $ Some
        SupportedPreimageHash_Blake2b_224
    | fn == "sha256" = Right $ Some
        SupportedPreimageHash_SHA256
    | otherwise = Left ()

withSupportedPreimageHash
  :: Some SupportedPreimageHash
  -> (forall h. HashAlgorithm h => SupportedPreimageHash h -> x)
  -> x
withSupportedPreimageHash sm k = case sm of
    Some sh -> case sh of
        h@(SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash h) -> k h
        h@(SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash h) -> k h
        h@(SupportedPreimageHash_SHA256 :: SupportedPreimageHash h) -> k h

parseRegistryEntry
    :: Aeson.Value
    -> Aeson.Parser (WithOwnership Maybe PartialGoguenRegistryEntry)
parseRegistryEntry = Aeson.withObject "GoguenRegistryEntry" $ \o -> do
    subject <- o .:? "subject"
    nameField <- o .:? "name"
    descriptionField <- o .:? "description"
    preimageField <- o .:? "preImage"
    logoField <- o .:? "logo"
    ownershipField <- o .:? "owner"
    nameAnn <- mapM (parseWithAttestation parseWellKnownGoguen) nameField
    descAnn <- mapM (parseWithAttestation parseWellKnownGoguen) descriptionField
    logo <- mapM (parseWithAttestation parseWellKnownGoguen) logoField
    preimage <- mapM parseRegistryPreimage preimageField
    owner <- mapM (parseAnnotatedSignature OwnershipSignature) ownershipField
    pure $ WithOwnership
        { _withOwnership_value = GoguenRegistryEntry
            { _goguenRegistryEntry_subject = Subject <$> subject
            , _goguenRegistryEntry_name = nameAnn
            , _goguenRegistryEntry_description = descAnn
            , _goguenRegistryEntry_logo = logo
            , _goguenRegistryEntry_preimage = preimage
            }
      , _withOwnership_owner = owner
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

parseRegistryPreimage
    :: Aeson.Value
    -> Aeson.Parser Preimage
parseRegistryPreimage = Aeson.withObject "preImage" $ \o -> do
    hashFn <- Aeson.prependFailure "preimage " $
        o .: "hashFn"
    preimage <- Aeson.prependFailure "preimage " $
        o .: "value"
    AesonHelpers.noOtherFields "preimage" o ["hashFn", "value"]
    pure $ Preimage
        { _preimage_hashFn = hashFn
        , _preimage_preimage = preimage
        }

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

verifyPreimage
    :: (Monoid s, IsString s)
    => Subject
    -> Preimage
    -> Either s ()
verifyPreimage (Subject subjectHex) preimage =
    case T.decodeHex subjectHex of
        Nothing ->
            Left "Subject is not a byte string"
        Just subject -> do
            hasher <- case fromHashFnIdentifier (_preimage_hashFn preimage) of
                Right (Some h) -> Right $ case h of
                    (SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash h) ->
                        hashToBytes . hashWith @h id
                    (SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash h) ->
                        hashToBytes . hashWith @h id
                    (SupportedPreimageHash_SHA256 :: SupportedPreimageHash h) ->
                        hashToBytes . hashWith @h id
                Left () -> Left $ mconcat
                    [ "Hash function not supported. Supported functions: "
                    , "sha256, blake2b-256, blake2b-224"
                    ]
            case T.decodeHex (_preimage_preimage preimage) of
                Nothing ->
                    Left "Preimage is not a byte string"
                Just preimageBytes -> do
                    if hasher preimageBytes == subject
                    then pure ()
                    else Left "Hashed preimage does not equal subject"

hashPreimage
  :: (Monoid s, IsString s)
  => Preimage
  -> Either s Subject
hashPreimage preimage = do
    hasher <- case fromHashFnIdentifier (_preimage_hashFn preimage) of
        Right (Some h) -> Right $ case h of
            (SupportedPreimageHash_Blake2b_256 :: SupportedPreimageHash h) ->
                hashToBytes . hashWith @h id
            (SupportedPreimageHash_Blake2b_224 :: SupportedPreimageHash h) ->
                hashToBytes . hashWith @h id
            (SupportedPreimageHash_SHA256 :: SupportedPreimageHash h) ->
                hashToBytes . hashWith @h id
        Left () -> Left $ mconcat
            [ "Hash function not supported. Supported functions: "
            , "sha256, blake2b-256, blake2b-224"
            ]
    case T.decodeHex (_preimage_preimage preimage) of
        Nothing -> Left "Preimage is not a byte string"
        Just preimageBytes -> pure $ Subject $ T.encodeHex $ hasher preimageBytes

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

-- | The goguen metadata registry does something slightly different to compute the ownership hash.
-- namely, it interprets the attestation signatures as hexademical strings and encodes them into UTF-8
-- instead of hashing their bytestring representation direcctly.
registryHashesForOwnership
    :: Subject
    -> Map Property (Attested PropertyValue)
    -> HashesForOwnership
registryHashesForOwnership s ps = HashesForOwnership
    { _hashesForOwnership_subject = hashSubject s
    , _hashesForOwnership_properties = flip Map.mapWithKey ps $ \p av ->
            ( hashProperty p
            , hashPropertyValue (_attested_property av)
            , Map.fromList $ flip fmap (_attested_signatures av) $ \sig ->
                ( Tagged (rawSerialiseVerKeyDSIGN (_attestationSignature_publicKey sig))
                , hashWith (T.encodeUtf8 . T.encodeHex . rawSerialiseSigDSIGN) (_attestationSignature_signature sig)
                )
            )
    }

verifyRegistryOwnership
  :: WithOwnership Maybe PartialGoguenRegistryEntry
  -> Either String ()
verifyRegistryOwnership ownedEntry = do
    let grabMaybe field = \case
            Just x -> pure x
            Nothing -> Left $ "Verifying owner signature: missing: " <> field
    let entry :: PartialGoguenRegistryEntry = _withOwnership_value ownedEntry
    subject <- grabMaybe "subject" $ _goguenRegistryEntry_subject entry
    name <- grabMaybe "name" $ _goguenRegistryEntry_name entry
    description <- grabMaybe "description" $ _goguenRegistryEntry_description entry
    owner <- grabMaybe "owner signature" $ _withOwnership_owner ownedEntry
    case _goguenRegistryEntry_logo entry of
        Just _ -> Left "Currently, owner signatures are not supported on records with logos"
        Nothing -> Right ()
    let hashes = registryHashesForOwnership subject $ Map.fromList
          [ withWellKnown (_attested_property name) $ \p _ -> (p, fmap _wellKnown_raw name)
          , withWellKnown (_attested_property description) $ \p _ -> (p, fmap _wellKnown_raw description)
          ]
    left (const "Ownership signature did not verify") $ verifyOwnership $ WithOwnership
        { _withOwnership_owner = Identity owner
        , _withOwnership_value = hashes
        }

serializeRegistryEntry
    :: WithOwnership Maybe PartialGoguenRegistryEntry
    -> PP.Doc ann
serializeRegistryEntry entry = encloseObj $ catMaybes
    [ flip fmap (_goguenRegistryEntry_subject entry') $ \subject -> objField "subject" $
        PP.dquotes $ PP.pretty $ unSubject subject
    , flip fmap (_goguenRegistryEntry_preimage entry') $ \preimage -> objField "preImage" $ encloseObj $
        [ objField "value" $ PP.dquotes (PP.pretty (_preimage_preimage preimage))
        , objField "hashFn" $ PP.dquotes (PP.pretty (_preimage_hashFn preimage))
        ]
    , flip fmap (_goguenRegistryEntry_name entry') $ \name -> objField "name" $
        attested prettyWellKnown name
    , flip fmap (_goguenRegistryEntry_description entry') $ \description -> objField "description" $
        attested prettyWellKnown description
    , flip fmap (_goguenRegistryEntry_logo entry') $ \logo -> objField "logo" $
        attested prettyWellKnown logo
    , flip fmap (_withOwnership_owner entry) $ \owner -> objField "owner" $
        ownership owner
    ]
 where
    entry' = _withOwnership_value entry
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
    ownership s = encloseObj
        [ objField "publicKey" $ prettyBytes $ rawSerialiseVerKeyDSIGN $
            _ownershipSignature_publicKey s
        , objField "signature" $ prettyBytes $ rawSerialiseSigDSIGN $
            _ownershipSignature_signature s
        ]
