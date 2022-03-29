{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.CheckMode
  ( runChecker
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad (forever, forM_, when)
import qualified Data.Map.Strict as M
import           Debug.Trace (traceIO)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Types

-- | When 'cardano-tracer' is launched with '--check-mode' flag, it
--   prints out all 'EKG.Metrics' and 'TraceObject's it receives from
--   the node(s). We use it for debug purposes.
runChecker
  :: Bool
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> IO ()
runChecker itIsCheckMode acceptedMetrics savedTO =
  when itIsCheckMode doRunChecker
 where
  doRunChecker = forever $ do
    allMetrics <- M.toList <$> readTVarIO acceptedMetrics
    forM_ allMetrics $ \(nodeId, (ekgStore, _)) -> do
      metrics <- getListOfMetrics ekgStore
      traceIO "-----------------------"
      traceIO $ "All metrics from " <> show nodeId <> ": " <> show metrics
      traceIO "-----------------------"

    allTraceObjects <- M.toList <$> readTVarIO savedTO
    forM_ allTraceObjects $ \(nodeId, savedForNode) -> do
      traceIO "***********************"
      traceIO $ "All trace objects from " <> show nodeId <> ": " <> show savedForNode
      traceIO "***********************"

    sleep 1.0
