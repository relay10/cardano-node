{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Errors
  ( updateErrors
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Types

updateErrors
  :: UI.Window
  -> AcceptedMetrics
  -> UI ()
updateErrors _window acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(_nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(_metricName, _metricValue) ->
      return ()
