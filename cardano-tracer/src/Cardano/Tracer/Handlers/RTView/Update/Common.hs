{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Common
  ( runCommonUpdater
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever, forM_) 
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Text as T
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Handlers.RTView.State.Common
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

runCommonUpdater
  :: ConnectedNodes
  -> NodesEraSettings
  -> SavedTraceObjects
  -> IO ()
runCommonUpdater connectedNodes nodesSettings savedTO = forever $ do
  connected <- readTVarIO connectedNodes
  updateNodesEraSettings connected nodesSettings savedTO
  sleep 1.0

updateNodesEraSettings
  :: Set NodeId
  -> NodesEraSettings
  -> SavedTraceObjects
  -> IO ()
updateNodesEraSettings connected nodesSettings savedTO = do
  savedTraceObjects <- readTVarIO savedTO
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode ->
      whenJust (M.lookup "Cardano.Node.Startup.ShelleyBased" savedTOForNode) $ \(trObValue, _, _) ->
        -- Example: "Era Alonzo, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600"
        case T.words $ T.replace "," "" trObValue of
          [_, era, _, _, slotLen, _, _, epochLen, _, _, _, kesPeriod] ->
            addNodeEraSettings nodesSettings nodeId $
              NodeEraSettings
                { nesEra             = era
                , nesSlotLengthInS   = readInt (T.init slotLen) 0
                , nesEpochLength     = readInt epochLen 0
                , nesKESPeriodLength = readInt kesPeriod 0
                }
          _ -> return ()
