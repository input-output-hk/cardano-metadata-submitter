{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Metadata.Types
    ( Subject (..)
    , hashSubject
    , Policy (..)
    , hashPolicy
    , Property (..)
    , hashProperty
    , PropertyValue (..)
    , propertyValueFromString
    , propertyValueToString
    , propertyValueToJson
    , hashPropertyValue
    , AttestationSignature (..)
    , makeAttestationSignature
    , HashesForAttestation (..)
    , hashesForAttestation
    , attestationDigest
    , Attested (..)
    , emptyAttested
    , isAttestedBy
    , verifyAttested
    , WellKnownProperty (..)
    , WellKnown (..)
    , parseWellKnown'
    , withWellKnown
    , Name (..)
    , Description (..)
    , Logo (..)
    , Url(..)
    , Ticker(..)
    , Unit(..)
    ) where

import Cardano.Prelude

import Cardano.Crypto.DSIGN
    ( Ed25519DSIGN
    , SigDSIGN
    , SignKeyDSIGN
    , VerKeyDSIGN
    , deriveVerKeyDSIGN
    , signDSIGN
    , verifyDSIGN
    )
import Cardano.Crypto.Hash
    ( Blake2b_224, Blake2b_256, Hash, castHash, hashToBytes, hashWith )
import Control.Category
    ( id )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( (.:) )
import Data.Text
    ( Text )
import Network.URI
    ( URI (..), parseAbsoluteURI )

import qualified AesonHelpers
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Subject = Subject { unSubject :: Text }
    deriving Show

hashSubject :: Subject -> Hash Blake2b_256 Subject
hashSubject = hashWith (T.encodeUtf8 . unSubject)

newtype Policy = Policy { unPolicy :: Text }
    deriving Show

hashPolicy :: Policy -> Hash Blake2b_224 Policy
hashPolicy = hashWith (T.encodeUtf8 . unPolicy)

newtype Property = Property { unProperty :: Text }
    deriving (Show, Eq, Ord)

hashProperty :: Property -> Hash Blake2b_256 Property
hashProperty = hashWith (T.encodeUtf8 . unProperty)

-- | Metadata property values must be parseable as JSON, but they are utf-8 encoded strings.
data PropertyValue = PropertyValue
    { _propertyValue_raw :: Text
    , _propertyValue_parse :: Aeson.Value
    } deriving Show

-- | Smart constructor guards creation of 'PropertyValue's so that we can
-- verify that they are valid JSON. 'Left' signals an aeson error.
propertyValueFromString :: Text -> Either Text PropertyValue
propertyValueFromString t = bimap T.pack (PropertyValue t) $
    Aeson.eitherDecodeStrict @Aeson.Value (T.encodeUtf8 t)

propertyValueToString :: PropertyValue -> Text
propertyValueToString = _propertyValue_raw

propertyValueToJson :: PropertyValue -> Aeson.Value
propertyValueToJson = _propertyValue_parse

hashPropertyValue :: PropertyValue -> Hash Blake2b_256 PropertyValue
hashPropertyValue = hashWith (T.encodeUtf8 . propertyValueToString)

-- | An 'AttestationSignature is a pair of a public key and a signature
-- that can be verified with that public key of a message derived from
-- the subject, property name, and property value being attested to.
--
-- In particular, the message is hash(hash(subject) + hash (property_name) + hash(property_value))
data AttestationSignature = AttestationSignature
    { _attestationSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
    , _attestationSignature_signature :: SigDSIGN Ed25519DSIGN
    } deriving Show

makeAttestationSignature
    :: SignKeyDSIGN Ed25519DSIGN
    -> HashesForAttestation
    -> AttestationSignature
makeAttestationSignature signingKey hashes = AttestationSignature
    { _attestationSignature_publicKey = deriveVerKeyDSIGN signingKey
    , _attestationSignature_signature = signDSIGN ()
        (hashToBytes $ attestationDigest hashes)
        signingKey
    }

-- | Hashes required to produce a message for attestation purposes
data HashesForAttestation = HashesForAttestation
    { _hashesForAttestation_subject :: Hash Blake2b_256 Subject
    , _hashesForAttestation_property :: Hash Blake2b_256 Property
    , _hashesForAttestation_value :: Hash Blake2b_256 PropertyValue
    }

hashesForAttestation
 :: Subject
 -> Property
 -> PropertyValue
 -> HashesForAttestation
hashesForAttestation s p v = HashesForAttestation
    { _hashesForAttestation_subject = hashSubject s
    , _hashesForAttestation_property = hashProperty p
    , _hashesForAttestation_value = hashPropertyValue v
    }

attestationDigest
    :: HashesForAttestation
    -> Hash Blake2b_256 HashesForAttestation
attestationDigest hashes = castHash $ hashWith id $ mconcat
    [ hashToBytes $ _hashesForAttestation_subject hashes
    , hashToBytes $ _hashesForAttestation_property hashes
    , hashToBytes $ _hashesForAttestation_value hashes
    ]

-- | Metadata entries can be provided along with annotated signatures
-- attesting to the validity of those entry values.
data Attested a = Attested
    { _attested_signatures :: [AttestationSignature]
    , _attested_property :: a
    } deriving (Functor, Show)

emptyAttested
    :: a
    -> Attested a
emptyAttested a = Attested
    { _attested_signatures = []
    , _attested_property = a
    }

isAttestedBy
    :: HashesForAttestation
    -> AttestationSignature
    -> Either Text ()
isAttestedBy hashes sig = first T.pack $ verifyDSIGN ()
    (_attestationSignature_publicKey sig)
    (hashToBytes (attestationDigest hashes))
    (_attestationSignature_signature sig)

verifyAttested
    :: Attested HashesForAttestation
    -> Either [AttestationSignature] ()
verifyAttested attested =
    let
        (invalids, _) = partitionEithers $ flip fmap (_attested_signatures attested) $ \sig ->
            first (const sig) $ isAttestedBy (_attested_property attested) sig
    in
        case invalids of
            [] -> Right ()
            _ -> Left invalids

-- TODO: Add a tag type that allows all well known properties to be enumerated
class WellKnownProperty p where
    wellKnownPropertyName :: f p -> Property
    parseWellKnown :: PropertyValue -> Aeson.Parser p

data WellKnown p where
    WellKnown :: WellKnownProperty p =>
        { _wellKnown_raw :: PropertyValue
        , _wellKnown_structured :: p
        } -> WellKnown p

deriving instance Show p => Show (WellKnown p)

parseWellKnown'
    :: WellKnownProperty p
    => Aeson.Value
    -> Aeson.Parser (WellKnown p)
parseWellKnown' =
    let
        parse t =
            case propertyValueFromString t of
                Left err -> fail (T.unpack err)
                Right v -> WellKnown v <$> parseWellKnown v
     in
        Aeson.withText "property value" parse

withWellKnown
    :: WellKnownProperty p
    => WellKnown p
    -> (Property -> PropertyValue -> x)
    -> x
withWellKnown p f =
    f (wellKnownPropertyName p) (_wellKnown_raw p)

-- | "name" is a well-known property whose value must be a string
newtype Name = Name { unName :: Text }
    deriving Show

instance WellKnownProperty Name where
    wellKnownPropertyName _ = Property "name"
    parseWellKnown = Aeson.withText "name"
        validateMetadataName . propertyValueToJson

-- | "description" is a well-known property whose value must be a string
newtype Description = Description { unDescription :: Text }
    deriving Show

instance WellKnownProperty Description where
    wellKnownPropertyName _ = Property "description"
    parseWellKnown = Aeson.withText "description"
        validateMetadataDescription . propertyValueToJson

data Logo = Logo
    { _logo_png_contents :: BL.ByteString
    } deriving Show

instance WellKnownProperty Logo where
    wellKnownPropertyName _ = Property "logo"
    parseWellKnown = Aeson.withText "logo"
        (either fail (validateMetadataLogo . BL.fromStrict) . B64.decode . T.encodeUtf8) .  propertyValueToJson

newtype Url = Url
    { unUrl :: URI
    } deriving Show

instance WellKnownProperty Url where
    wellKnownPropertyName _ = Property "url"
    parseWellKnown = Aeson.withText "url"
        validateMetadataURL . propertyValueToJson

data Unit = Unit
    { unitName :: Text
    , unitDecimals :: Integer
    } deriving Show

instance WellKnownProperty Unit where
    wellKnownPropertyName _ = Property "unit"
    parseWellKnown =
        Aeson.withObject "unit" parser . propertyValueToJson
      where
        parser o = do
            name <- Aeson.prependFailure "unit" (o .: "name")
            decimals <- Aeson.prependFailure "unit" (o .: "decimals")
            AesonHelpers.noOtherFields "unit" o [ "name", "decimals" ]
            validateMetadataUnit name decimals

newtype Ticker = Ticker
    { unTicker :: Text
    } deriving Show

instance WellKnownProperty Ticker where
    wellKnownPropertyName _ = Property "ticker"
    parseWellKnown = Aeson.withText "ticker"
        validateMetadataTicker . propertyValueToJson


--
-- Validators
--

validateMinLength :: MonadFail f => Int -> Text -> f Text
validateMinLength n text
    | len >= n = pure text
    | otherwise = fail $ "Length must be at least " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMaxLength :: MonadFail f => Int -> Text -> f Text
validateMaxLength n text
    | len <= n = pure text
    | otherwise = fail $ "Length must be no more than " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMetadataName :: MonadFail f => Text -> f Name
validateMetadataName =  fmap Name .
    (validateMinLength 1 >=> validateMaxLength 50)

validateMetadataTicker :: MonadFail f => Text -> f Ticker
validateMetadataTicker = fmap Ticker .
    (validateMinLength 2 >=> validateMaxLength 4)

validateMetadataDescription :: MonadFail f => Text -> f Description
validateMetadataDescription = fmap Description .
    validateMaxLength 500

-- | FIXME: validate decimals
validateMetadataUnit :: MonadFail f => Text -> Integer -> f Unit
validateMetadataUnit name decimals = Unit name decimals <$
    (validateMinLength 1 name >>= validateMaxLength 30)

validateMetadataLogo :: MonadFail f => BL.ByteString -> f Logo
validateMetadataLogo bytes
    | len <= maxLen = pure (Logo bytes)
    | otherwise = fail $ "Length must be no more than " ++ show maxLen ++ " bytes, got " ++ show len
  where
    len = BL.length bytes
    maxLen = 65536

validateMetadataURL :: MonadFail f => Text -> f Url
validateMetadataURL = fmap Url .
    (validateMaxLength 250 >=> validateURI >=> validateHttps)
  where
      validateURI = maybe (fail "Not an absolute URI") pure
          . parseAbsoluteURI
          . T.unpack
      validateHttps u@(uriScheme -> scheme)
          | scheme == "https:" = pure u
          | otherwise = fail $ "Scheme must be https: but got " ++ scheme
