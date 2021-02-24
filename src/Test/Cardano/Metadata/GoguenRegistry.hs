{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Cardano.Crypto.Hash
    ( HashAlgorithm )
import Cardano.Metadata.GoguenRegistry
    ( CompleteGoguenRegistryEntry
    , GoguenRegistryEntry (..)
    , SupportedPreimageHash (..)
    , parseRegistryEntry
    , partialToCompleteRegistryEntry
    , serializeRegistryEntry
    , toHashFnIdentifier
    , withSupportedPreimageHash
    )
import Cardano.Metadata.Types
    ( Preimage (..), Subject (..), emptyAttested, partialToCompleteOwnership )
import Data.Some
    ( Some (..) )
import Data.Tagged
    ( Tagged (..) )
import Test.Cardano.Metadata.Types
    ( genDescription, genHashSubject, genLogo, genName )
import Test.Tasty.QuickCheck hiding
    ( Property )

import qualified Data.Aeson.Internal as AInternal
import qualified Data.Aeson.Parser as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

genSupportedPreimageHash :: Gen (Some SupportedPreimageHash)
genSupportedPreimageHash = suchThatMap (choose @Int (0, 2)) $ \case
    0 -> Just $ Some SupportedPreimageHash_Blake2b_256
    1 -> Just $ Some SupportedPreimageHash_Blake2b_224
    2 -> Just $ Some SupportedPreimageHash_SHA256
    _ -> Nothing

genSubjectWithPreimage
    :: forall h. HashAlgorithm h
    => SupportedPreimageHash h
    -> Gen (Subject, Preimage)
genSubjectWithPreimage h = do
    (pretext, Tagged subject) <- genHashSubject @h
    pure $ (,) subject $ Preimage
        { _preimage_preimage = pretext
        , _preimage_hashFn = toHashFnIdentifier h
        }

genCompleteEntry :: Gen CompleteGoguenRegistryEntry
genCompleteEntry = do
    hashFn <- genSupportedPreimageHash
    (subject, preimage) <- withSupportedPreimageHash hashFn genSubjectWithPreimage
    name <- genName
    description <- genDescription
    logo <- genLogo
    pure $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Identity subject
        , _goguenRegistryEntry_name = Identity $ emptyAttested name
        , _goguenRegistryEntry_description = Identity $ emptyAttested description
        , _goguenRegistryEntry_logo = Identity $ emptyAttested logo
        , _goguenRegistryEntry_preimage = Identity preimage
        }

testParse :: IO ()
testParse = do
    testInput <- BS.readFile "test/9d6d2f903f80992106abe9ac38f121afd9f4305286834eb5f01e45752816b185.json"
    let p = Aeson.eitherDecodeStrictWith Aeson.json' (AInternal.iparse parseRegistryEntry) testInput
    Right parsed <- pure p
    Just _complete <- pure $ do
        e <- partialToCompleteOwnership parsed
        mapM partialToCompleteRegistryEntry e
    -- print $ verifyPreimage (_withOwnership_value complete)
    -- print $ verifyAttestations $ _withOwnership_value complete
    -- print $ verifyRegistryOwnership complete
    BS.writeFile "test.json" $ T.encodeUtf8 $ mconcat
        [ PP.renderStrict $ PP.layoutPretty PP.defaultLayoutOptions $ serializeRegistryEntry parsed
        , "\n"
        ]
