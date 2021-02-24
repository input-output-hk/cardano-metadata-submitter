{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Metadata.Types where

import Cardano.Prelude

import Cardano.Crypto.Hash
    ( HashAlgorithm, hashToBytes, hashWith )
import Cardano.Metadata.Types
    ( Description (..)
    , Logo (..)
    , Name (..)
    , Subject (..)
    , WellKnown (..)
    , WellKnownProperty (..)
    , parseWellKnown'
    )
import Data.Tagged
    ( Tagged (..) )
import Test.Tasty.QuickCheck hiding
    ( Property )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Hex as T

genSubject :: Gen Subject
genSubject = Subject . T.pack . getPrintableString <$> arbitrary

genHashSubject :: forall h. HashAlgorithm h => Gen (Text, Tagged h Subject)
genHashSubject = do
  preimage <- T.pack . getPrintableString <$> arbitrary
  let hashed = hashWith @h T.encodeUtf8 preimage
  pure $ (,) preimage $ Tagged @h $ Subject $ T.encodeHex $ hashToBytes hashed

genTextValue :: Gen Aeson.Value
genTextValue =
    printableStringToJson <$> arbitrary
  where
    printableStringToJson
        = Aeson.String
        . toStrict
        . Aeson.encodeToLazyText
        . Aeson.String
        . T.pack
        . getPrintableString

genWellKnownText :: WellKnownProperty p => Gen (WellKnown p)
genWellKnownText = suchThatMap genTextValue $ \s -> case Aeson.parse parseWellKnown' s of
  Aeson.Error _ -> Nothing
  Aeson.Success x -> Just x

genName :: Gen (WellKnown Name)
genName = genWellKnownText

genDescription :: Gen (WellKnown Description)
genDescription = genWellKnownText

genLogo :: Gen (WellKnown Logo)
genLogo = genWellKnownText
