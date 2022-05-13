{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Errors
  ( runErrorsUpdater
  , updateNodesErrors
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever, forM_, unless, void, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Time.Extra (sleep)

import           Cardano.Logging (SeverityS (..))

import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

runErrorsUpdater
  :: ConnectedNodes
  -> Errors
  -> SavedTraceObjects
  -> IO ()
runErrorsUpdater connectedNodes nodesErrors savedTO = forever $ do
  sleep 2.0
  connected <- readTVarIO connectedNodes
  savedTraceObjects <- readTVarIO savedTO
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode ->
      forM_ (M.toList savedTOForNode) $ \(_, trObInfo@(_, severity, _)) ->
        when (itIsError severity) $
          addError nodesErrors nodeId trObInfo
 where
  itIsError sev =
    case sev of
      Warning   -> True
      Error     -> True
      Critical  -> True
      Alert     -> True
      Emergency -> True
      _         -> False

-- | Update error messages in a corresponding modal window.
updateNodesErrors
  :: UI.Window
  -> ConnectedNodes
  -> Errors
  -> UI ()
updateNodesErrors window connectedNodes nodesErrors = do
  connected <- liftIO $ readTVarIO connectedNodes
  forM_ connected $ \nodeId@(NodeId anId) -> do
    errorsFromNode <- liftIO $ getErrors nodesErrors nodeId
    unless (null errorsFromNode) $ do
      -- Update errors number.
      setTextValue (anId <> "__node-errors-num") (showT $ length errorsFromNode)
      -- Add errors.
      forM_ errorsFromNode $ \(errorIx, (msg, sev, ts)) ->
        addErrorRow errorIx nodeId msg sev ts
      -- Enable 'Details' button.
      findAndSet (set UI.enabled True)
                 window $ anId <> "__node-errors-details-button"
 where
  addErrorRow errorIx nodeId@(NodeId anId) msg sev ts = do
    copyErrorIcon <- image "has-tooltip-multiline has-tooltip-top rt-view-copy-icon" copySVG
                           # set dataTooltip "Click to copy this error"
    on UI.click copyErrorIcon . const $
      copyTextToClipboard $ errorToCopy ts sev msg

    deleteErrorIcon <- image "has-tooltip-multiline has-tooltip-top rt-view-delete-icon" deleteSVG
                             # set dataTooltip "Click to delete this error"
    errorRow <-
      UI.tr #+
        [ UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span # set text (show sev)
            ]
        , UI.td #+
            [ UI.span # set text (T.unpack $ shortenMsg msg)
            ]
        , UI.td #+
            [ element copyErrorIcon
            , element deleteErrorIcon
            ]
        ]

    on UI.click deleteErrorIcon . const $ do
      UI.delete errorRow
      liftIO $ deleteError nodesErrors nodeId errorIx

    whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
      void $ element el #+
        [ element errorRow
        ]

  shortenMsg msg = if T.length msg > 50 then T.take 50 msg <> "..." else msg

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

  errorToCopy ts sev msg =
    "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack msg <> "]"


  --prepareSeverity sev =
  --  case sev of
  --    Debug     -> "D"
  --    Info      -> "I"
  --    Notice    -> "N"
  --    Warning   -> "W"
  --    Error     -> "Er"
  --    Critical  -> "C"
  --    Alert     -> "A"
  --    Emergency -> "Em"
