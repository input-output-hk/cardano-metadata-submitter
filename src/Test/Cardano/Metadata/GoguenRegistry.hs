{-# LANGUAGE TypeApplications #-}
module Test.Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import qualified Data.Aeson.Internal as AInternal
import qualified Data.Aeson.Parser as A
import qualified Data.ByteString as BS

import Cardano.Metadata.GoguenRegistry
import Cardano.Metadata.Types

import Control.Category
import Cardano.Crypto.Hash
import Cardano.Crypto.DSIGN
import qualified Text.Hex as T
import qualified Data.Text.Encoding as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

testParse :: IO ()
testParse = do
  testInput <- BS.readFile "test/9d6d2f903f80992106abe9ac38f121afd9f4305286834eb5f01e45752816b185.json"
  p <- pure $
    A.eitherDecodeStrictWith A.json' (AInternal.iparse parseRegistryEntry) testInput
  Right parsed <- pure p
  print $ verifyPreimage (_withOwnership_value parsed)
  print $ verifyAttestations $ _withOwnership_value parsed
  print $ verifyRegistryOwnership parsed
  BS.writeFile "test.json" $ T.encodeUtf8 $ mconcat
    [ PP.renderStrict $ PP.layoutPretty PP.defaultLayoutOptions $ serializeRegistryEntry parsed
    , "\n"
    ]

testSubject :: Text
testSubject = "9d6d2f903f80992106abe9ac38f121afd9f4305286834eb5f01e45752816b185"

testProperty :: Text
testProperty = "name"

testValue :: Text
testValue = "\"example name a\""

testHash :: Hash Blake2b_256 ByteString
testHash = hashWith id $ mconcat $ fmap hashToBytes
  [ hashWith @ Blake2b_256 T.encodeUtf8 testSubject
  , hashWith T.encodeUtf8 testProperty
  , hashWith T.encodeUtf8 testValue
  ]

testSignature = verifyDSIGN () verKey (hashToBytes testHash) sig

verKey :: VerKeyDSIGN Ed25519DSIGN
Just verKey = rawDeserialiseVerKeyDSIGN =<< T.decodeHex "0334c6056068b372494220479d7d9af6c4d33bd643d884e7f6d50f73c3d607d3"

sig :: SigDSIGN Ed25519DSIGN
Just sig = rawDeserialiseSigDSIGN =<< T.decodeHex "601652d82d057d399ef86dc4028b279fa851005c9e9ec7894bf219a7f15eb32d8affa9a6bf46554329bef79fbcb930df802c16bc8c695924e1a36a3afa921004"

