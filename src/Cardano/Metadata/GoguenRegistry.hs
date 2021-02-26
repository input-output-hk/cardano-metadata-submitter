{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Cardano.Metadata.Types
    ( Attested (..)
    , Description (..)
    , Logo (..)
    , Logo (..)
    , Name (..)
    , Policy (..)
    , Property (..)
    , Subject (..)
    , Ticker (..)
    , Unit (..)
    , Url (..)
    , WellKnownProperty (..)
    , parseWithAttestation
    )
import Data.Aeson
    ( ToJSON (..), (.:?), (.=) )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
--
-- Neither the policy nor The subject are attested.
-- Attestations attest to the relationship between the subject and the field
-- and thus already include the subject in the hash. The policy itself is used
-- to verify attestations.
data GoguenRegistryEntry f = GoguenRegistryEntry
    { _goguenRegistryEntry_subject :: f Subject
    , _goguenRegistryEntry_policy :: f Policy
    , _goguenRegistryEntry_name :: f (Attested Name)
    , _goguenRegistryEntry_description :: f (Attested Description)
    , _goguenRegistryEntry_logo :: f (Attested Logo)
    , _goguenRegistryEntry_url :: f (Attested Url)
    , _goguenRegistryEntry_unit :: f (Attested Unit)
    , _goguenRegistryEntry_ticker :: f (Attested Ticker)
    }

deriving instance
    ( Show (f Subject)
    , Show (f Policy)
    , Show (f (Attested Name))
    , Show (f (Attested Description))
    , Show (f (Attested Logo))
    , Show (f (Attested Url))
    , Show (f (Attested Unit))
    , Show (f (Attested Ticker))
    ) => Show (GoguenRegistryEntry f)

instance ToJSON (GoguenRegistryEntry Maybe) where
    toJSON r = Aeson.object $ mconcat
        [ [ "subject" .= _goguenRegistryEntry_subject r
          , unProperty (wellKnownPropertyName (Proxy @Policy)) .=
              (wellKnownToJSON <$> (_goguenRegistryEntry_policy r))
          , unProperty (wellKnownPropertyName (Proxy @Name)) .=
              (fmap wellKnownToJSON <$> (_goguenRegistryEntry_name r))
          , unProperty (wellKnownPropertyName (Proxy @Description)) .=
              (fmap wellKnownToJSON <$> (_goguenRegistryEntry_description r))
          ]
        , catMaybes
          [ (\x -> unProperty (wellKnownPropertyName (Proxy @Logo)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_logo r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Url)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_url r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Unit)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_unit r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Ticker)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_ticker r)
          ]
        ]

type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

parseRegistryEntry
    :: Aeson.Value
    -> Aeson.Parser PartialGoguenRegistryEntry
parseRegistryEntry = Aeson.withObject "GoguenRegistryEntry" $ \o -> do
    subject <- o .:? "subject"

    policyRaw <- o .:? unProperty (wellKnownPropertyName $ Proxy @Policy)
    policy <- mapM parseWellKnown policyRaw

    nameField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Name)
    descField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Description)
    logoField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Logo)
    urlField    <- o .:? unProperty (wellKnownPropertyName $ Proxy @Url)
    unitField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Unit)
    tickerField <- o .:? unProperty (wellKnownPropertyName $ Proxy @Ticker)

    nameAnn   <- mapM parseWithAttestation nameField
    descAnn   <- mapM parseWithAttestation descField
    logoAnn   <- mapM parseWithAttestation logoField
    urlAnn    <- mapM parseWithAttestation urlField
    unitAnn   <- mapM parseWithAttestation unitField
    tickerAnn <- mapM parseWithAttestation tickerField

    pure $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Subject <$> subject
        , _goguenRegistryEntry_policy  = policy
        , _goguenRegistryEntry_name = nameAnn
        , _goguenRegistryEntry_description = descAnn
        , _goguenRegistryEntry_logo = logoAnn
        , _goguenRegistryEntry_url = urlAnn
        , _goguenRegistryEntry_unit = unitAnn
        , _goguenRegistryEntry_ticker = tickerAnn
        }
