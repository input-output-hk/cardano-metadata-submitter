module Cardano.Metadata.Types where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Cardano.Prelude
import Cardano.Crypto.Hash
import Cardano.Crypto.DSIGN

-- | An 'AttestationSignature is a pair of a public key and a signature
-- that can be verified with that public key of a message derived from
-- the subject, property name, and property value being attested to.
--
-- In particular, the message is hash(hash(subject) + hash (property_name) + hash(property_value))
data AttestationSignature = AttestationSignature
  { _attestationSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
  , _attestationSignature_signature :: SignedDSIGN Ed25519DSIGN (Hash Blake2b_256 HashesForAttestation)
  }

-- | Hashes required to produce a message for attestation purposes
data HashesForAttestation = HashesForAttestation
  { _hashesForAttestation_subject :: Hash Blake2b_256 Text
  , _hashesForAttestation_property :: Hash Blake2b_256 Text
  , _hashesForAttestation_value :: Hash Blake2b_256 Aeson.Value
  }

-- | Metadata entries can be provided along with annotated signatures
-- attesting to the validity of those entry values.
data Attested a = Attested
  { _attested_signatures :: [AttestationSignature]
  , _attested_value :: a
  }

-- | "name" is a well-known property whose value must be a string
newtype Name = Name { unName :: Text }

-- | "description" is a well-known property whose value must be a string
newtype Description = Description { unDescription :: Text }

-- | "preimage" is a well-known property whose value must be a record two
-- fields: "hashFn" which names the hash function used to derive the metadata
-- subject and "preimage" which is the preimage of the hash subject under that
-- hash function.
--
-- NB: Metadata subjects are frequentely, but not always, hashes
data Preimage = Preimage
  { _preimage_preimage :: Text
  , _preimage_hashFn :: Text
  }

-- | The goguen-metadata-registry determines ownership by signing entries
-- with Ed25519.
data OwnershipSignature = OwnershipSignature
  { _OwnershipSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
  , _OwnershipSignature_signature :: SignedDSIGN Ed25519DSIGN (Hash Blake2b_256 HashesForOwnership)
  }

-- | Ownership signatures are hash( hash(subject)
-- + hash(property_name_1) + hash(property_value_1)
-- + hash(attestation_sig_1_1) + ... + hash(attestation_signature_1_k1)
-- + ... + hash(property_name_n) + hash(property_value_n)
-- + hash(attestation_sig_n_1) + ... + hash(attestation_sig_n_kn)
-- )
--
-- where properties are lexicographically ordered by name and signatures are
-- lexicographically ordered by their attesting key. That's what the 'Map's do.
data HashesForOwnership = HashesForOwnership
  { _hashesForOwnership_subject :: Hash Blake2b_256 Text
  , _hashesForOwnership_properties ::
    Map Text
      ( Hash Blake2b_256 Text
      , Hash Blake2b_256 Aeson.Value
      , Map (VerKeyDSIGN Ed25519DSIGN)
          (Hash Blake2b_256 (SignedDSIGN Ed25519DSIGN (Hash Blake2b_256 HashesForAttestation)))
      )
  }

data WithOwnership a = WithOwnership
  { _withOwnership_owner :: OwnershipSignature
  , _withOwnership_value :: a
  }

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to be exactly in this form.
data GoguenRegistryEntry = GoguenRegistryEntry
  { _goguenRegistryEntry_subject :: Text
  , _goguenRegistryEntry_name :: Attested Name
  , _goguenRegistryEntry_description :: Attested Description
  , _goguenRegistryEntry_preimage :: Attested Preimage
  }

