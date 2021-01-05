{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Control.Category
import Control.Monad.Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A hiding (parseEither)
import qualified Data.Text as T
import qualified Text.Hex as T

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash

import qualified AesonHelpers as A
import Cardano.Metadata.Types

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
data GoguenRegistryEntry f = GoguenRegistryEntry
  { _goguenRegistryEntry_subject :: f Subject
  , _goguenRegistryEntry_name :: f (Attested (WellKnown Name))
  , _goguenRegistryEntry_description :: f (Attested (WellKnown Description))
  , _goguenRegistryEntry_preimage :: f Preimage
  -- ^ The preimage is not attested because it is directly verifiable.
  }

deriving instance (Show (f Subject), Show (f (Attested (WellKnown Name))), Show (f (Attested (WellKnown Description))), Show (f Preimage)) => Show (GoguenRegistryEntry f)

type CompleteGoguenRegistryEntry = GoguenRegistryEntry Identity
type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

parseRegistryEntry
  :: A.Value
  -> A.Parser (WithOwnership CompleteGoguenRegistryEntry)
parseRegistryEntry = A.withObject "GoguenRegistryEntry" $ \o -> do
  subject <- o A..: "subject"
  nameField <- o A..: "name"
  descriptionField <- o A..: "description"
  preimageField <- o A..: "preImage"
  ownershipField <- o A..: "owner"
  nameAnn <- parseWithAttestation parseWellKnownGoguen nameField
  descAnn <- parseWithAttestation parseWellKnownGoguen descriptionField
  preimage <- parseRegistryPreimage preimageField
  owner <- parseAnnotatedSignature OwnershipSignature ownershipField
  pure $ WithOwnership
    { _withOwnership_value = GoguenRegistryEntry
      { _goguenRegistryEntry_subject = Subject <$> subject
      , _goguenRegistryEntry_name = Identity nameAnn
      , _goguenRegistryEntry_description = Identity descAnn
      , _goguenRegistryEntry_preimage = Identity preimage
      }
    , _withOwnership_owner = owner
    }


-- | The registry encodes "name" and "description" differently from the CIP.
-- in particular `{ "value": "Foo" }` instead of `{ "value" : "\"Foo\"" }`.
parseWellKnownGoguen
  :: WellKnownProperty p
  => A.Value
  -> A.Parser (WellKnown p)
parseWellKnownGoguen =
  let parse t = case propertyValueFromString ("\"" <> t <> "\"") of
        Left err -> fail (T.unpack err)
        Right v -> WellKnown v <$> parseWellKnown v
   in A.withText "property value" parse

parseRegistryPreimage
  :: A.Value
  -> A.Parser Preimage
parseRegistryPreimage = A.withObject "preImage" $ \o -> do
  hashFn <- A.prependFailure "preimage " $
    o A..: "hashFn"
  preimage <- A.prependFailure "preimage " $
    o A..: "value"
  A.noOtherFields "preimage" o ["hashFn", "value"]
  pure $ Preimage
    { _preimage_hashFn = hashFn
    , _preimage_preimage = preimage
    }

parseWithAttestation
  :: (A.Value -> A.Parser a)
  -> A.Value
  -> A.Parser (Attested a)
parseWithAttestation parseValue = A.withObject "property with attestation" $ \o -> do
  value <- parseValue =<< o A..: "value"
  attestations <- (o A..: "anSignatures" >>=) $ A.withArray "Annotated Signatures" $
    fmap toList . mapM (A.withObject "Attestation" (parseAnnotatedSignature AttestationSignature))
  pure $ Attested
    { _attested_signatures = attestations
    , _attested_property = value
    }

parseAnnotatedSignature
  :: DSIGNAlgorithm v
  => (VerKeyDSIGN v -> SigDSIGN v -> x)
  -> A.Object
  -> A.Parser x
parseAnnotatedSignature f o = do
  publicKeyField <- o A..: "publicKey"
  signatureField <- o A..: "signature"
  publicKey <- flip (A.withText "publicKey") publicKeyField $ \t ->
    maybe (fail $ T.unpack $ "Couldn't parse verification key: " <> t) pure $
      rawDeserialiseVerKeyDSIGN =<< T.decodeHex t
  signature <- flip (A.withText "signature") signatureField $ \t ->
    maybe (fail $ T.unpack $ "Couldn't parse signature " <> t) pure $
      rawDeserialiseSigDSIGN =<< T.decodeHex t
  A.noOtherFields "annotated signature" o ["publicKey", "signature"]
  pure $ f publicKey signature

verifyPreimage
  :: CompleteGoguenRegistryEntry
  -> Either () ()
verifyPreimage entry =
  case T.decodeHex (unSubject (runIdentity (_goguenRegistryEntry_subject entry))) of
    Nothing -> Left ()
    Just subject -> do
      let Identity preimage = _goguenRegistryEntry_preimage entry
      hasher <- case _preimage_hashFn preimage of
        fn | fn == "blake2b-256" -> Right $ hashToBytes . hashWith @Blake2b_256 id
           | fn == "blake2b-224" -> Right $ hashToBytes . hashWith @Blake2b_224 id
           | fn == "sha256" -> Right $ hashToBytes . hashWith @SHA256 id
           | otherwise -> Left ()
      case T.decodeHex (_preimage_preimage preimage) of
        Nothing -> Left ()
        Just preimageBytes -> do
          case hasher preimageBytes == subject of
            False -> Left ()
            True -> pure ()

verifyAttestations
  :: CompleteGoguenRegistryEntry
  -> Either [AttestationSignature] ()
verifyAttestations entry = do
  let Identity subject = _goguenRegistryEntry_subject entry
      Identity name = _goguenRegistryEntry_name entry
      Identity description = _goguenRegistryEntry_description entry
  _ <- verifyAttested $ fmap (withWellKnown (hashesForAttestation subject)) name
  _ <- verifyAttested $ fmap (withWellKnown (hashesForAttestation subject)) description
  pure ()
