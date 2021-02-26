{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Metadata.Types
    ( Subject (..)
    , hashSubject
    , Policy (..)
    , hashPolicy
    , evaluatePolicy
    , Property (..)
    , hashProperty
    , PropertyValue (..)
    , propertyValueFromString
    , propertyValueToString
    , propertyValueToJson
    , hashPropertyValue
    , AttestationSignature (..)
    , MakeAttestationSignature(..)
    , SomeSigningKey (..)
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
    , parseUnit
    ) where

import Cardano.Prelude

import Cardano.Api
    ( AsType (AsMaryEra, AsScriptInEra)
    , MaryEra
    , PaymentExtendedKey
    , PaymentKey
    , Script (..)
    , ScriptHash
    , ScriptInEra (..)
    , SigningKey
    , SimpleScript
    , SimpleScriptVersion (..)
    , deserialiseFromCBOR
    , hashScript
    , serialiseToRawBytes
    )
import Cardano.Crypto.DSIGN
    ( Ed25519DSIGN
    , SigDSIGN
    , VerKeyDSIGN
    , deriveVerKeyDSIGN
    , rawDeserialiseSigDSIGN
    , rawDeserialiseSignKeyDSIGN
    , rawDeserialiseVerKeyDSIGN
    , signDSIGN
    , verifyDSIGN
    )
import Cardano.Crypto.Hash
    ( Blake2b_256, Hash, castHash, hashToBytes, hashWith )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( Timelock (RequireAllOf, RequireAnyOf, RequireMOf, RequireSignature, RequireTimeExpire, RequireTimeStart)
    , ValidityInterval (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Codec.Picture.Png
    ( decodePng )
import Control.Category
    ( id )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( (.:) )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Data.Text.Read
    ( decimal )
import Network.URI
    ( URI (..), parseAbsoluteURI )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (SJust, SNothing) )
import Shelley.Spec.Ledger.Keys
    ( KeyHash (..), KeyRole (Witness) )

import qualified AesonHelpers
import qualified Cardano.Api as Api
import qualified Cardano.Crypto.Wallet as CC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.Keys as Shelley

newtype Subject = Subject { unSubject :: Text }
    deriving Show

hashSubject :: Subject -> Hash Blake2b_256 Subject
hashSubject = hashWith (T.encodeUtf8 . unSubject)

data Policy = Policy
    { rawPolicy :: Text
    , getPolicy :: ScriptInEra MaryEra
    } deriving Show

hashPolicy :: Policy -> ScriptHash
hashPolicy (Policy _ (ScriptInEra _ script)) =
    hashScript script

instance WellKnownProperty Policy where
    wellKnownPropertyName _ = Property "policy"
    parseWellKnown = Aeson.withText "policy"
        validateMetadataPolicy . propertyValueToJson

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

data SomeSigningKey where
    SomeSigningKey
        :: forall keyrole. (MakeAttestationSignature keyrole)
        => SigningKey keyrole
        -> SomeSigningKey

class MakeAttestationSignature keyrole where
    makeAttestationSignature
        :: SigningKey keyrole
        -> HashesForAttestation
        -> AttestationSignature

instance MakeAttestationSignature PaymentKey where
    makeAttestationSignature key hashes =
        AttestationSignature
            { _attestationSignature_publicKey =
                deriveVerKeyDSIGN prv
            , _attestationSignature_signature = signDSIGN ()
                (hashToBytes $ attestationDigest hashes)
                prv
            }
      where
        -- Very ugly cast of a 'PaymentKey' into a SignKeyDSign
        Just prv = rawDeserialiseSignKeyDSIGN (serialiseToRawBytes key)

instance MakeAttestationSignature PaymentExtendedKey where
    makeAttestationSignature key hashes =
        AttestationSignature
            { _attestationSignature_publicKey = unsafeToVerKeyDSign
                $ CC.toXPub xprv
            , _attestationSignature_signature = unsafeToSigDSign
                $ CC.sign (mempty :: ByteString) xprv (hashToBytes $ attestationDigest hashes)
            }
      where
        -- Very ugly cast of a 'PaymentExtendedKey' into an 'XPrv'
        Right xprv = CC.xprv (serialiseToRawBytes key)
        -- NOTE: We can 'safely' cast to VerKeyDSIGN and SigDSIGN for
        -- verification because the signature verification algorithm is the
        -- same for extended and normal keys.
        unsafeToVerKeyDSign = fromJust . rawDeserialiseVerKeyDSIGN . CC.xpubPublicKey
        unsafeToSigDSign = fromJust . rawDeserialiseSigDSIGN . CC.unXSignature

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

evaluatePolicy
    :: Policy
    -> SlotNo
    -> [AttestationSignature]
    -> Either Text ()
evaluatePolicy (Policy _ script) atSlot sigs =
    case script of
        ScriptInEra _ (SimpleScript SimpleScriptV1 s) ->
            evaluateAtSlot $ toAllegraTimelock s
        ScriptInEra _ (SimpleScript SimpleScriptV2 s) ->
            evaluateAtSlot $ toAllegraTimelock s
        ScriptInEra _ (PlutusScript _ _) ->
            panic "impossible"
  where
    evaluateAtSlot :: Timelock StandardCrypto -> Either Text ()
    evaluateAtSlot s
        | evalTimelock hashes (ValidityInterval (SJust atSlot) (SJust atSlot)) s =
            Right ()
        | otherwise =
            Left "Unable to validate the monetary policy now and with current attestations."

    hashes :: Set (KeyHash 'Witness StandardCrypto)
    hashes = Set.fromList
        $ Shelley.hashKey . Shelley.VKey . _attestationSignature_publicKey <$> sigs

    -- | Conversion for the 'Timelock.Timelock' language that is shared between the
    -- Allegra and Mary eras.
    --
    toAllegraTimelock :: forall lang. SimpleScript lang -> Timelock StandardCrypto
    toAllegraTimelock = go
      where
        go :: SimpleScript lang -> Timelock StandardCrypto
        go (Api.RequireSignature (Api.PaymentKeyHash kh)) = RequireSignature (Shelley.coerceKeyRole kh)
        go (Api.RequireAllOf s) = RequireAllOf (Seq.fromList (map go s))
        go (Api.RequireAnyOf s) = RequireAnyOf (Seq.fromList (map go s))
        go (Api.RequireMOf m s) = RequireMOf m (Seq.fromList (map go s))
        go (Api.RequireTimeBefore _ t) = RequireTimeExpire t
        go (Api.RequireTimeAfter  _ t) = RequireTimeStart  t

    evalTimelock
        :: (crypto ~ StandardCrypto)
        => Set (KeyHash 'Witness crypto)
        -> ValidityInterval
        -> Timelock crypto
        -> Bool
    evalTimelock _vhks (ValidityInterval start _) (RequireTimeStart lockStart) =
        lockStart `lteNegInfty` start
    evalTimelock _vhks (ValidityInterval _ end) (RequireTimeExpire lockExp) =
        end `ltePosInfty` lockExp
    evalTimelock vhks _vi (RequireSignature hash) =
        Set.member hash vhks
    evalTimelock vhks vi (RequireAllOf xs) =
        all (evalTimelock vhks vi) xs
    evalTimelock vhks vi (RequireAnyOf xs) =
        any (evalTimelock vhks vi) xs
    evalTimelock vhks vi (RequireMOf m xs) =
        m <= sum (fmap (\x -> if evalTimelock vhks vi x then 1 else 0) xs)

    -- | less-than-equal comparison, where Nothing is negative infinity
    lteNegInfty :: SlotNo -> StrictMaybe SlotNo -> Bool
    lteNegInfty _ SNothing = False -- i > -∞
    lteNegInfty i (SJust j) = i <= j

    -- | less-than-equal comparison, where Nothing is positive infinity
    ltePosInfty :: StrictMaybe SlotNo -> SlotNo -> Bool
    ltePosInfty SNothing _ = False -- ∞ > j
    ltePosInfty (SJust i) j = i <= j

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
        parseUnit . propertyValueToJson

parseUnit
    :: Aeson.Value
    -> Aeson.Parser Unit
parseUnit v =
    parseAsText v <|> parseAsObject v
  where
    parseAsText = Aeson.withText "unit" $ \t -> case decimal t of
        Left e -> fail e
        Right (decimals, name) ->
            validateMetadataUnit (T.drop 1 name) decimals

    parseAsObject = Aeson.withObject "unit" $ \o -> do
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

validateMinimum :: MonadFail f => (Ord a, Show a) => a -> a -> f a
validateMinimum minN n
    | n > minN = pure n
    | otherwise = fail $ "Value must be strictly greater than " ++ show minN

validateMaximum :: MonadFail f => (Ord a, Show a) => a -> a -> f a
validateMaximum maxN n
    | n < maxN = pure n
    | otherwise = fail $ "Value must be strictly smaller than " ++ show maxN

validateBase16 :: MonadFail f => Text -> f ByteString
validateBase16 =
    either fail pure . B16.decode . T.encodeUtf8

validateMetadataName :: MonadFail f => Text -> f Name
validateMetadataName = fmap Name .
    (validateMinLength 1 >=> validateMaxLength 50)

validateScriptInEra :: MonadFail f => ByteString -> f (ScriptInEra MaryEra)
validateScriptInEra =
    either (fail . show) pure . deserialiseFromCBOR (AsScriptInEra AsMaryEra)

validateMetadataPolicy :: MonadFail f => Text -> f Policy
validateMetadataPolicy t = Policy t <$>
    (validateBase16 >=> validateScriptInEra) t

validateMetadataTicker :: MonadFail f => Text -> f Ticker
validateMetadataTicker = fmap Ticker .
    (validateMinLength 2 >=> validateMaxLength 4)

validateMetadataDescription :: MonadFail f => Text -> f Description
validateMetadataDescription = fmap Description .
    validateMaxLength 500

validateMetadataUnit :: MonadFail f => Text -> Integer -> f Unit
validateMetadataUnit name decimals = Unit name decimals <$
    (  validateMinLength 1 name >>= validateMaxLength 30
    >> validateMaximum 20 decimals >>= validateMinimum 0
    )

validateMetadataLogo :: MonadFail f => BL.ByteString -> f Logo
validateMetadataLogo bytes
    | len <= maxLen =
        case decodePng (BL.toStrict bytes) of
            Left e     -> fail $ "Verifying PNG: " <> e
            Right _png -> pure (Logo bytes)
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
