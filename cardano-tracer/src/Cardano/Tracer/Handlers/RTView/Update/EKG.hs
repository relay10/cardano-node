{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.EKG
  ( updateEKGMetrics
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, unless)
import qualified Data.Map.Strict as M
import           Data.Text (intercalate)
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

updateEKGMetrics :: AcceptedMetrics -> UI ()
updateEKGMetrics acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(NodeId anId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    unless (null metrics) $ do
      setTextValue (anId <> "__node-ekg-metrics-num") (showT $ length metrics)
      let (mNames, mValues) = unzip metrics
      setTextValue (anId <> "__node-ekg-metrics-names")  (intercalate "<br>" mNames)
      setTextValue (anId <> "__node-ekg-metrics-values") (intercalate "<br>" mValues)
