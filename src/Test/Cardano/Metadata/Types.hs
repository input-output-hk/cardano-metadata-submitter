{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Cardano.Metadata.Types where

import Cardano.Prelude

import Cardano.Crypto.Hash
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import qualified Text.Hex as T
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Tasty.QuickCheck hiding (Property)

import Cardano.Metadata.Types

genSubject :: Gen Subject
genSubject = Subject . T.pack . getPrintableString <$> arbitrary

genHashSubject :: forall h. HashAlgorithm h => Gen (Text, Tagged h Subject)
genHashSubject = do
  preimage <- T.pack . getPrintableString <$> arbitrary
  let hashed = hashWith @h T.encodeUtf8 preimage
  pure $ (,) preimage $ Tagged @h $ Subject $ T.encodeHex $ hashToBytes hashed

genTextValue :: Gen A.Value
genTextValue = A.String . toStrict . A.encodeToLazyText . A.String . T.pack . getPrintableString <$> arbitrary

genWellKnownText :: WellKnownProperty p => Gen (WellKnown p)
genWellKnownText = suchThatMap genTextValue $ \s -> case A.parse parseWellKnown' s of
  A.Error _ -> Nothing
  A.Success x -> Just x

genName :: Gen (WellKnown Name)
genName = genWellKnownText

genDescription :: Gen (WellKnown Description)
genDescription = genWellKnownText
