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
import qualified Data.Map as Map
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Hex as T
import qualified Prettyprinter as PP

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash

import qualified AesonHelpers as A
import Cardano.Metadata.Types

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
data GoguenRegistryEntry f = GoguenRegistryEntry
  { _goguenRegistryEntry_subject :: f Subject
  -- ^ The subject is not separately attested because it is signed in all attestations
  , _goguenRegistryEntry_name :: f (Attested (WellKnown Name))
  , _goguenRegistryEntry_description :: f (Attested (WellKnown Description))
  , _goguenRegistryEntry_preimage :: f Preimage
  -- ^ The preimage is not attested because it is directly verifiable.
  }

deriving instance (Show (f Subject), Show (f (Attested (WellKnown Name))), Show (f (Attested (WellKnown Description))), Show (f Preimage)) => Show (GoguenRegistryEntry f)

type CompleteGoguenRegistryEntry = GoguenRegistryEntry Identity
type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

completeToPartialRegistryEntry
  :: CompleteGoguenRegistryEntry
  -> PartialGoguenRegistryEntry
completeToPartialRegistryEntry e = GoguenRegistryEntry
  { _goguenRegistryEntry_subject = Just $ runIdentity $ _goguenRegistryEntry_subject e
  , _goguenRegistryEntry_name = Just $ runIdentity $ _goguenRegistryEntry_name e
  , _goguenRegistryEntry_description = Just $ runIdentity $ _goguenRegistryEntry_description e
  , _goguenRegistryEntry_preimage = Just $ runIdentity $ _goguenRegistryEntry_preimage e
  }

partialToCompleteRegistryEntry
  :: PartialGoguenRegistryEntry
  -> Maybe CompleteGoguenRegistryEntry
partialToCompleteRegistryEntry e = GoguenRegistryEntry
  <$> fmap Identity (_goguenRegistryEntry_subject e)
  <*> fmap Identity (_goguenRegistryEntry_name e)
  <*> fmap Identity (_goguenRegistryEntry_description e)
  <*> fmap Identity (_goguenRegistryEntry_preimage e)

parseRegistryEntry
  :: A.Value
  -> A.Parser (WithOwnership Maybe PartialGoguenRegistryEntry)
parseRegistryEntry = A.withObject "GoguenRegistryEntry" $ \o -> do
  subject <- o A..:? "subject"
  nameField <- o A..:? "name"
  descriptionField <- o A..:? "description"
  preimageField <- o A..:? "preImage"
  ownershipField <- o A..:? "owner"
  nameAnn <- mapM (parseWithAttestation parseWellKnownGoguen) nameField
  descAnn <- mapM (parseWithAttestation parseWellKnownGoguen) descriptionField
  preimage <- mapM parseRegistryPreimage preimageField
  owner <- mapM (parseAnnotatedSignature OwnershipSignature) ownershipField
  pure $ WithOwnership
    { _withOwnership_value = GoguenRegistryEntry
      { _goguenRegistryEntry_subject = Subject <$> subject
      , _goguenRegistryEntry_name = nameAnn
      , _goguenRegistryEntry_description = descAnn
      , _goguenRegistryEntry_preimage = preimage
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
  _ <- verifyAttested $ fmap (flip withWellKnown (hashesForAttestation subject)) name
  _ <- verifyAttested $ fmap (flip withWellKnown (hashesForAttestation subject)) description
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
  :: WithOwnership Identity CompleteGoguenRegistryEntry
  -> Either () ()
verifyRegistryOwnership ownedEntry = verifyOwnership $ flip fmap ownedEntry $ \entry ->
  let Identity subject = _goguenRegistryEntry_subject entry
      Identity name = _goguenRegistryEntry_name entry
      Identity description = _goguenRegistryEntry_description entry
   in registryHashesForOwnership subject $ Map.fromList
        [ withWellKnown (_attested_property name) $ \p _ -> (p, fmap _wellKnown_raw name)
        , withWellKnown (_attested_property description) $ \p _ -> (p, fmap _wellKnown_raw description)
        ]

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
