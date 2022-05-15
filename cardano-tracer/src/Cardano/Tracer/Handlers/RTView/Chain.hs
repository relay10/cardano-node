{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Chain
  ( EraName
  , EpochNum
  , FirstEpochInEra
  , EraStartPOSIX
  , epochsInfo
  ) where

import           Data.Text
import           Data.Word (Word64)

type EraName         = Text
type EpochNum        = Int
type FirstEpochInEra = EpochNum
type EraStartPOSIX   = Word64

-- It is taken 'cardano-ledger' wiki topic "First-Block-of-Each-Era".
epochsInfo :: [(EraName, (EraStartPOSIX, FirstEpochInEra))]
epochsInfo =
  [ ("Shelley", (1596073491, 208)) -- 07/30/2020 1:44:51 AM GMT
  , ("Allegra", (1608169491, 236)) -- 12/17/2020 1:44:51 AM GMT
  , ("Mary",    (1614649491, 251)) -- 03/02/2021 1:44:51 AM GMT
  , ("Alonzo",  (1634953491, 298)) -- 10/23/2021 1:44:51 AM GMT, start of new protocol.
  ]
