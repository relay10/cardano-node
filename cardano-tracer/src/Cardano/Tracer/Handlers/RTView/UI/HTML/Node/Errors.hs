{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Errors
  ( mkErrorsTable
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkErrorsTable :: String -> UI Element
mkErrorsTable anId = do
  closeIt <- UI.button #. "delete"
  errorsTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-errors-modal" #+
          [ UI.header #. "modal-card-head rt-view-errors-head" #+
              [ UI.p #. "modal-card-title rt-view-errors-title" #+
                  [ string "Errors from "
                  , UI.span ## (anId <> "__node-name-for-errors")
                            #. "has-text-weight-bold"
                            # set text anId
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-errors-body" #+
              [ UI.div ## (anId <> "__errors-table-container") #. "table-container" #+
                  [ UI.table ## (anId <> "__errors-table") #. "table is-fullwidth rt-view-errors-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #+ [UI.span # set text "Time"]
                              , UI.th #+ [UI.span # set text "Severity"]
                              , UI.th #+ [UI.span # set text "Message"]
                              , UI.th #+ [UI.span # set text "Actions"]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (anId <> "__node-errors-tbody") #+ []
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element errorsTable #. "modal"
  return errorsTable
