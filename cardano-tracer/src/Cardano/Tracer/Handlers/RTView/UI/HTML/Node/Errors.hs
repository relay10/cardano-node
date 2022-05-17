{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Errors
  ( mkErrorsTable
  ) where

import           Control.Monad (when)
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Errors
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
    deleteAllErrorMessages window nodeId nodesErrors

  searchMessagesInput <- UI.input #. "input rt-view-search-messages"
                                  # set UI.type_ "text"
                                  # set (UI.attr "placeholder") "Search messages"
  searchMessages <- UI.button #. "button is-info"
                              #+ [image "rt-view-search-errors-icon" searchSVG]
  -- If the user clicked the search button.
  on UI.click searchMessages . const $
    searchErrorMessages window searchMessagesInput nodeId nodesErrors
  -- If the user hits Enter key.
  on UI.keyup searchMessagesInput $ \keyCode ->
    when (keyCode == 13) $
      searchErrorMessages window searchMessagesInput nodeId nodesErrors

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
                              [ UI.th #. "rt-view-errors-timestamp" #+
                                  [ string "Timestamp"
                                  ]
                              , UI.th #. "rt-view-errors-severity" #+
                                  [ string "Severity"
                                  ]
                              , UI.th #+
                                  [ string "Message"
                                  ]
                              , UI.th #+
                                  [ element deleteAll
                                  ]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (id' <> "__node-errors-tbody")
                                             # set dataState "0"
                                             #+ []
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-errors-foot" #+
              [ UI.div #. "field has-addons" #+
                  [ UI.p #. "control" #+
                      [ element searchMessagesInput
                      ]
                  , UI.p #. "control" #+
                      [ element searchMessages
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element errorsTable #. "modal"
  return errorsTable
