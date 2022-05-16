{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Errors
  ( runErrorsUpdater
  , updateNodesErrors
  , searchErrorMessages
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Time.Extra (sleep)
import           Text.Read (readMaybe)

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
      -- Update errors number (as it is in the state).
      setTextValue (anId <> "__node-errors-num") (showT $ length errorsFromNode)
      -- Enable 'Details' button.
      findAndSet (set UI.enabled True) window (anId <> "__node-errors-details-button")
      -- Add errors if needed.
      whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
        whenJustM (readMaybe <$> get dataState el) $ \(numberOfDisplayedRows :: Int) -> do
          let onlyNewErrors = drop numberOfDisplayedRows errorsFromNode
          doAddErrorRows nodeId onlyNewErrors el numberOfDisplayedRows

doAddErrorRows
  :: NodeId
  -> [ErrorInfo]
  -> Element
  -> Int
  -> UI ()
doAddErrorRows nodeId errorsToAdd parentEl numberOfDisplayedRows = do
  errorRows <-
    forM errorsToAdd $ \(errorIx, (msg, sev, ts)) ->
      mkErrorRow errorIx nodeId msg sev ts
  -- Add them actually and remember their new number.
  let newNumberOfDisplayedRows = numberOfDisplayedRows + length errorsToAdd
  void $ element parentEl # set dataState (show newNumberOfDisplayedRows)
                          #+ errorRows
 where
  mkErrorRow _errorIx (NodeId anId) msg sev ts = do
    copyErrorIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                           # set dataTooltip "Click to copy this error"
    on UI.click copyErrorIcon . const $
      copyTextToClipboard $ errorToCopy ts sev msg

    return $
      UI.tr #. (T.unpack anId <> "-node-error-row") #+
        [ UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span #. "tag is-medium is-danger" # set text (show sev)
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack msg)
                ]
            ]
        , UI.td #+
            [ element copyErrorIcon
            ]
        ]

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

  errorToCopy ts sev msg = "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack msg <> "]"

searchErrorMessages
  :: UI.Window
  -> Text
  -> NodeId
  -> Errors
  -> UI ()
searchErrorMessages window textToSearch nodeId@(NodeId anId) nodesErrors =
  liftIO (getErrorsFilteredByText textToSearch nodesErrors nodeId) >>= \case
    [] -> do
      -- There is nothing found. So we have to display an empty list of
      -- errors to inform the user that there is no corresponding errors.
      findByClassAndDo window (anId <> "-node-error-row") UI.delete
      -- Reset number of currently displayed errors rows.
      whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
        void $ element el # set dataState "0"
    foundErrors -> do
      -- Delete displayed errors from window.
      findByClassAndDo window (anId <> "-node-error-row") UI.delete
      -- Do add found errors.
      whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
        whenJustM (readMaybe <$> get dataState el) $ \(numberOfDisplayedRows :: Int) ->
          doAddErrorRows nodeId foundErrors el numberOfDisplayedRows
