{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Handlers.RTView.State.Errors
  ( ErrorIx
  , ErrorInfo
  , Errors
  , addError
  , getError
  , getErrors
  , getErrorsFilteredBySeverity
  , getErrorsFilteredByText
  , getErrorsSortedBy
  , timeAsc
  , timeDesc
  , severityAsc
  , severityDesc 
  , initErrors
  , deleteError
  , deleteAllErrors
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Data.List (delete, find, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text, isInfixOf)

import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Types (NodeId)

import           Cardano.Logging (SeverityS)

type ErrorIx = Int
type ErrorInfo = (ErrorIx, TraceObjectInfo)
type Errors = TVar (Map NodeId [ErrorInfo])

initErrors :: IO Errors
initErrors = newTVarIO M.empty

addError
  :: Errors
  -> NodeId
  -> TraceObjectInfo
  -> IO ()
addError errors nodeId trObInfo = atomically $
  modifyTVar' errors $ \currentErrors ->
    case M.lookup nodeId currentErrors of
      Nothing -> do
        let errorIx = 0
        M.insert nodeId [(errorIx, trObInfo)] currentErrors
      Just errorsFromNode -> do
        let errorIx = length errorsFromNode
        M.adjust (const $ errorsFromNode ++ [(errorIx, trObInfo)]) nodeId currentErrors

deleteError
  :: Errors
  -> NodeId
  -> ErrorIx
  -> IO ()
deleteError errors nodeId errorIx = atomically $
  modifyTVar' errors $ \currentErrors ->
    case M.lookup nodeId currentErrors of
      Nothing -> currentErrors
      Just errorsFromNode ->
        case find errorByIx errorsFromNode of
          Nothing -> currentErrors
          Just errorWeNeed -> do
            let updatedErrors = delete errorWeNeed errorsFromNode
            M.adjust (const updatedErrors) nodeId currentErrors
 where
  errorByIx (ix, _) = ix == errorIx

deleteAllErrors
  :: Errors
  -> NodeId
  -> IO ()
deleteAllErrors errors nodeId = atomically $
  modifyTVar' errors $ \currentErrors ->
    case M.lookup nodeId currentErrors of
      Nothing -> currentErrors
      Just _ -> M.adjust (const []) nodeId currentErrors

getError
  :: ErrorIx
  -> Errors
  -> NodeId
  -> IO (Maybe ErrorInfo)
getError errorIx errors nodeId =
  getErrors errors nodeId >>= \case
    [] -> return Nothing
    allErrors -> return $ find (\(ix, _) -> ix == errorIx) allErrors

getErrors
  :: Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrors errors nodeId =
  getErrorsHandled errors nodeId id

getErrorsSortedBy
  :: (ErrorInfo -> ErrorInfo -> Ordering)
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsSortedBy ordering errors nodeId =
  getErrorsHandled errors nodeId $ sortBy ordering

timeAsc
  , timeDesc
  , severityAsc
  , severityDesc :: ErrorInfo -> ErrorInfo -> Ordering
timeAsc      (_, (_, _,  ts1)) (_, (_, _,  ts2)) = ts1 `compare` ts2
timeDesc     (_, (_, _,  ts1)) (_, (_, _,  ts2)) = ts2 `compare` ts1
severityAsc  (_, (_, s1, _))   (_, (_, s2, _))   = s1  `compare` s2
severityDesc (_, (_, s1, _))   (_, (_, s2, _))   = s2  `compare` s1

getErrorsFilteredBySeverity
  :: SeverityS
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsFilteredBySeverity severity errors nodeId =
  getErrorsHandled errors nodeId $ filter (\(_, (_, sev, _)) -> sev == severity)

getErrorsFilteredByText
  :: Text
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsFilteredByText textToSearch errors nodeId =
  getErrorsHandled errors nodeId $ filter (\(_, (msg, _, _)) -> textToSearch `isInfixOf` msg)

getErrorsHandled
  :: Errors
  -> NodeId
  -> ([ErrorInfo] -> [ErrorInfo])
  -> IO [ErrorInfo]
getErrorsHandled errors nodeId handler = 
  (M.lookup nodeId <$> readTVarIO errors) >>= \case
    Nothing -> return []
    Just errorsFromNode -> return $ handler errorsFromNode
