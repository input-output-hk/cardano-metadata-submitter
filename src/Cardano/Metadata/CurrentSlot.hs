{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Metadata.CurrentSlot
    ( getCurrentSlot
    , mainnetSlotParameters
    , testnetSlotParameters
    ) where

import Cardano.Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Data.Time
    ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )

import qualified Prelude

data SlotParameters = SlotParameters
    { startTime :: UTCTime
    , startSlot :: SlotNo
    , slotLength :: NominalDiffTime
    } deriving Show

-- | Quick'n'dirty calculation of the current slot based of the network's
-- shelley start time. This only "works" because the slot length doesn't change
-- between Shelley-Allegra-Mary.
getCurrentSlot :: SlotParameters -> IO SlotNo
getCurrentSlot SlotParameters{startTime,startSlot,slotLength} = do
    now <- getCurrentTime
    let delta = nominalDiffTimeToSeconds $ now `diffUTCTime` startTime
    let slotsSinceShelley = fromIntegral $ delta `div` round slotLength
    pure $ SlotNo $ slotsSinceShelley + unSlotNo startSlot
  where
    nominalDiffTimeToSeconds :: NominalDiffTime -> Integer
    nominalDiffTimeToSeconds = round

--
-- Mainnet
--

mainnetSlotParameters :: SlotParameters
mainnetSlotParameters = SlotParameters
    mainnetShelleyStartTime
    mainnetShelleyStartSlot
    mainnetShelleySlotLength
  where
    mainnetShelleyStartTime :: UTCTime
    mainnetShelleyStartTime = Prelude.read "2020-07-29 21:44:51 UTC"

    mainnetShelleyStartSlot :: SlotNo
    mainnetShelleyStartSlot = SlotNo 4492800

    mainnetShelleySlotLength :: NominalDiffTime
    mainnetShelleySlotLength = 1

--
-- Testnet
--

testnetSlotParameters :: SlotParameters
testnetSlotParameters = SlotParameters
    testnetShelleyStartTime
    testnetShelleyStartSlot
    testnetShelleySlotLength
  where
    testnetShelleyStartTime :: UTCTime
    testnetShelleyStartTime = Prelude.read "2020-07-28 20:20:16 UTC"

    testnetShelleyStartSlot :: SlotNo
    testnetShelleyStartSlot = SlotNo 1598400

    testnetShelleySlotLength :: NominalDiffTime
    testnetShelleySlotLength = 1
