{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.KES
  ( updateKESInfo
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Common
import           Cardano.Tracer.Types

updateKESInfo
  :: UI.Window
  -> AcceptedMetrics
  -> NodesEraSettings
  -> UI ()
updateKESInfo _window acceptedMetrics _nodesEraSettings = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(_nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, _metricValue) ->
      case metricName of
        "cardano.node.currentKESPeriod" ->
          return ()
        "cardano.node.operationalCertificateExpiryKESPeriod" ->
          return ()
        "cardano.node.operationalCertificateStartKESPeriod" ->
          return ()
        "cardano.node.remainingKESPeriods" ->
          return ()
        _ -> return ()
