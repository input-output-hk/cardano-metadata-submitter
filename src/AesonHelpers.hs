module AesonHelpers where

import Cardano.Prelude

import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet

import Control.Category
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

parseEither
  :: (a -> A.Parser b)
  -> a
  -> Either Text b
parseEither = (bimap T.pack id .) . A.parseEither

noOtherFields
  :: Text
  -- ^ "type" of the object for error reporting purposes
  -> A.Object
  -> [Text]
  -- ^ Fields we expect
  -> A.Parser ()
noOtherFields tp o fs = A.modifyFailure (\_ -> T.unpack tp <> " contained extraneous fields") $
  guard $ HSet.null $ HMap.keysSet o `HSet.difference` HSet.fromList fs
