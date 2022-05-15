{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.State.Common
  ( NodeEraSettings (..)
  , NodesEraSettings
  , addNodeEraSettings
  , initNodesEraSettings
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           Cardano.Tracer.Types (NodeId)

data NodeEraSettings = NodeEraSettings
  { nesEra             :: !Text
  , nesSlotLengthInS   :: !Int
  , nesEpochLength     :: !Int
  , nesKESPeriodLength :: !Int
  } deriving (Eq, Show)

type NodesEraSettings = TVar (Map NodeId NodeEraSettings)

initNodesEraSettings :: IO NodesEraSettings
initNodesEraSettings = newTVarIO M.empty

addNodeEraSettings
  :: NodesEraSettings
  -> NodeId
  -> NodeEraSettings
  -> IO ()
addNodeEraSettings nodesSettings nodeId settingsForIt = atomically $
  modifyTVar' nodesSettings $ \currentSettings ->
    case M.lookup nodeId currentSettings of
      Nothing ->
        M.insert nodeId settingsForIt currentSettings
      Just savedSettings ->
        -- The settings for the same era shouldn't be changed.
        if nesEra savedSettings == nesEra settingsForIt
          then currentSettings
          else M.adjust (const settingsForIt) nodeId currentSettings
