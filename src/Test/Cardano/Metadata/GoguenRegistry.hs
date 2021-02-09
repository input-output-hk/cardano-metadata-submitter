{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import qualified Data.Aeson.Internal as AInternal
import qualified Data.Aeson.Parser as A
import qualified Data.ByteString as BS

import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types

import Cardano.Crypto.Hash
import Data.Some
import Data.Tagged
import qualified Data.Text.Encoding as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Test.Tasty.QuickCheck hiding (Property)

import Test.Cardano.Metadata.Types

genSupportedPreimageHash :: Gen (Some SupportedPreimageHash)
genSupportedPreimageHash = suchThatMap (choose @Int (0, 2)) $ \i -> case i of
  0 -> Just $ Some $ SupportedPreimageHash_Blake2b_256
  1 -> Just $ Some $ SupportedPreimageHash_Blake2b_224
  2 -> Just $ Some $ SupportedPreimageHash_SHA256
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
    , _goguenRegistryEntry_preimage = Identity $ preimage
    }

testParse :: IO ()
testParse = do
  testInput <- BS.readFile "test/9d6d2f903f80992106abe9ac38f121afd9f4305286834eb5f01e45752816b185.json"
  p <- pure $
    A.eitherDecodeStrictWith A.json' (AInternal.iparse parseRegistryEntry) testInput
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


