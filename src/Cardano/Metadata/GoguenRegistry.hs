{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Control.Monad.Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Internal as AInternal
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A hiding (parseEither)
import qualified Data.Text as T
import qualified Text.Hex as T

import Cardano.Crypto.DSIGN.Class

import qualified AesonHelpers as A
import Cardano.Metadata.Types

import qualified Data.ByteString as BS

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
data GoguenRegistryEntry f = GoguenRegistryEntry
  { _goguenRegistryEntry_subject :: f Subject
  , _goguenRegistryEntry_name :: f (Attested Name)
  , _goguenRegistryEntry_description :: f (Attested Description)
  , _goguenRegistryEntry_preimage :: f Preimage
  -- ^ The preimage is not attested because it is directly verifiable.
  }

deriving instance (Show (f Subject), Show (f (Attested Name)), Show (f (Attested Description)), Show (f Preimage)) => Show (GoguenRegistryEntry f)

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
  nameAnn <- parseWithAttestation parseWellKnown nameField
  descAnn <- parseWithAttestation parseWellKnown descriptionField
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

parseRegistryPreimage
  :: A.Value
  -> A.Parser Preimage
parseRegistryPreimage = A.withObject "preImage" $ \o -> do
  hashFn <- A.prependFailure "preimage " $
    o A..: "hashFn"
  preimage <- A.prependFailure "preimage " $
    o A..: "hex"
  A.noOtherFields "preimage" o ["hashFn", "hex"]
  pure $ Preimage
    { _preimage_hashFn = hashFn
    , _preimage_preimage = preimage
    }

parseWithAttestation
  :: (A.Value -> A.Parser a)
  -> A.Value
  -> A.Parser (Attested a)
parseWithAttestation parseValue = A.withObject "Property with Attestation" $ \o -> do
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
