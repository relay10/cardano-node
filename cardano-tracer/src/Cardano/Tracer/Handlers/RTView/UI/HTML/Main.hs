{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Main
  ( mkMainPage
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenM)
import           Data.List.NonEmpty (NonEmpty)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Common
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Peers
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Body
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.ChartJS
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.EKG
import           Cardano.Tracer.Handlers.RTView.Update.KES
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Update.NodeState
import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Handlers.RTView.Update.Reload
import           Cardano.Tracer.Types

mkMainPage
  :: ConnectedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> NodesEraSettings
  -> DataPointRequestors
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> Network
  -> ResourcesHistory
  -> BlockchainHistory
  -> TransactionsHistory
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedElements acceptedMetrics savedTO
           nodesEraSettings dpRequestors reloadFlag loggingConfig networkConfig
           resourcesHistory chainHistory txHistory window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ UI.link # set UI.rel "icon"
              # set UI.href ("data:image/svg+xml;base64," <> faviconSVGBase64)
    , UI.meta # set UI.name "viewport"
              # set UI.content "width=device-width, initial-scale=1"
    , UI.mkElement "style"  # set UI.html bulmaCSS
    , UI.mkElement "style"  # set UI.html bulmaTooltipCSS
    , UI.mkElement "style"  # set UI.html bulmaPageloaderCSS
    , UI.mkElement "style"  # set UI.html ownCSS
    , UI.mkElement "script" # set UI.html chartJS
    , UI.mkElement "script" # set UI.html chartJSLuxon
    , UI.mkElement "script" # set UI.html chartJSAdapter
    , UI.mkElement "script" # set UI.html chartJSPluginZoom
    ]

  colors <- initColors
  datasetIndices <- initDatasetsIndices
  datasetTimestamps <- initDatasetsTimestamps
  peers <- liftIO initPeers

  pageBody <-
    mkPageBody
      window
      networkConfig
      connectedNodes
      resourcesHistory
      chainHistory
      txHistory
      datasetIndices
      datasetTimestamps

  -- Prepare and run the timer, which will hide the page preloader.
  preloaderTimer <- UI.timer # set UI.interval 10
  on UI.tick preloaderTimer . const $ do
    liftIO $ sleep 0.8
    findAndSet (set UI.class_ "pageloader") window "preloader"
    UI.stop preloaderTimer
  UI.start preloaderTimer

  restoreTheme window
  restoreChartsSettings

  whenM (liftIO $ readTVarIO reloadFlag) $ do
    updateUIAfterReload
      window
      connectedNodes
      displayedElements
      dpRequestors
      loggingConfig
      colors
      datasetIndices
    liftIO $ pageWasNotReload reloadFlag

  -- Uptime is a real-time clock, so update it every second.
  uiUptimeTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiUptimeTimer . const $
    updateNodesUptime connectedNodes displayedElements
  UI.start uiUptimeTimer

  uiEKGTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiEKGTimer . const $
    updateEKGMetrics acceptedMetrics
  UI.start uiEKGTimer

  uiNodesTimer <- UI.timer # set UI.interval 1500
  on UI.tick uiNodesTimer . const $
    updateNodesUI
      window
      connectedNodes
      displayedElements
      acceptedMetrics
      savedTO
      nodesEraSettings
      dpRequestors
      loggingConfig
      colors
      datasetIndices
  UI.start uiNodesTimer

  uiPeersTimer <- UI.timer # set UI.interval 3000
  on UI.tick uiPeersTimer . const $ do
    updateNodesPeers window peers savedTO
    updateKESInfo window acceptedMetrics nodesEraSettings displayedElements
  UI.start uiPeersTimer

  uiNodeStateTimer <- UI.timer # set UI.interval 5000
  on UI.tick uiNodeStateTimer . const $
    askNSetNodeState window connectedNodes dpRequestors displayedElements
  UI.start uiNodeStateTimer

  on UI.disconnect window . const $ do
    UI.stop uiNodesTimer
    UI.stop uiUptimeTimer
    UI.stop uiPeersTimer
    UI.stop uiNodeStateTimer
    UI.stop uiEKGTimer
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody
