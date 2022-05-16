{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Errors
  ( mkErrorsTable
  ) where

import           Data.Text (unpack) 
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkErrorsTable
  :: UI.Window
  -> NodeId
  -> Errors
  -> UI Element
mkErrorsTable window nodeId@(NodeId anId) nodesErrors = do
  let id' = unpack anId
  closeIt <- UI.button #. "delete"
  deleteAll <- image "has-tooltip-multiline has-tooltip-left rt-view-delete-errors-icon" trashSVG
                     # set dataTooltip "Click to delete all errors. This action cannot be undone!"
  on UI.click deleteAll . const $
    deleteAllErrorMessages

  errorsTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-errors-modal" #+
          [ UI.header #. "modal-card-head rt-view-errors-head" #+
              [ UI.p #. "modal-card-title rt-view-errors-title" #+
                  [ string "Errors from "
                  , UI.span ## (id' <> "__node-name-for-errors")
                            #. "has-text-weight-bold"
                            # set text id'
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-errors-body" #+
              [ UI.div ## (id' <> "__errors-table-container") #. "table-container" #+
                  [ UI.table ## (id' <> "__errors-table") #. "table is-fullwidth rt-view-errors-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #+ [string "Timestamp"]
                              , UI.th #+ [string "Severity"]
                              , UI.th #+ [string
                              "Message"]
                              , UI.th #+
                                  [ element deleteAll
                                  ]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (id' <> "__node-errors-tbody") #+ []
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element errorsTable #. "modal"
  return errorsTable
 where
  deleteAllErrorMessages = do
    -- Delete errors from window.
    findByClassAndDo window (anId <> "-node-error-row") UI.delete
    -- Delete errors from state.
    liftIO $ deleteAllErrors nodesErrors nodeId
