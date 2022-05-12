{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  ) where

import           Control.Monad (forM, void)
import           Control.Monad.Extra (whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath ((</>))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.EKG
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | For every connected node the new column should be added.
addNodeColumn
  :: UI.Window
  -> NonEmpty LoggingParams
  -> NodeId
  -> UI ()
addNodeColumn window loggingConfig (NodeId anId) = do
  let id' = unpack anId
  itIsDarkTheme <- isCurrentThemeDark
  ls <- logsSettings loggingConfig id' itIsDarkTheme

  peersTable <- mkPeersTable id'
  peersDetailsButton <- UI.button ## (id' <> "__node-peers-details-button")
                                  #. "button is-info"
                                  # set UI.enabled False
                                  # set text "Details"
  on UI.click peersDetailsButton . const $ element peersTable #. "modal is-active" 

  ekgMetricsWindow <- mkEKGMetricsWindow id'
  ekgMetricsButton <- UI.button ## (id' <> "__node-ekg-metrics-button")
                                #. "button is-info"
                                # set text "Details"
  on UI.click ekgMetricsButton . const $ element ekgMetricsWindow #. "modal is-active"

  addNodeCellH "name"    [ image "rt-view-node-chart-label has-tooltip-multiline has-tooltip-left" rectangleSVG
                                 ## (id' <> "__node-chart-label")
                                 # set dataTooltip "Label using for this node on charts"
                         , UI.span ## (id' <> "__node-name")
                                   #. "has-text-weight-bold is-size-4 rt-view-node-name"
                                   # set text "Node"
                         ]
  addNodeCell "version"  [ UI.span ## (id' <> "__node-version")
                                   # set text "—"
                         ]
  addNodeCell "commit"   [ UI.anchor ## (id' <> "__node-commit")
                                     #. ("rt-view-href is-family-monospace has-text-weight-normal"
                                         <> " has-tooltip-multiline has-tooltip-right")
                                     # set UI.href "#"
                                     # set UI.target "_blank"
                                     # set dataTooltip "Browse cardano-node repository on this commit"
                                     # set text "—"
                         , image "rt-view-href-icon" externalLinkSVG
                         ]
  addNodeCell "protocol" [ UI.span ## (id' <> "__node-protocol")
                                   # set text "—"
                         ]
  addNodeCell "era" [ UI.span ## (id' <> "__node-era")
                              # set text "—"
                    ]
  addNodeCell "epoch" [ string "#"
                      , UI.span ## (id' <> "__node-epoch-num") # set text "—"
                      , image "has-tooltip-multiline has-tooltip-top rt-view-epoch-end" endSVG
                              # set dataTooltip "End date of this epoch"
                      , UI.span ## (id' <> "__node-epoch-end") # set text "—"
                      ]
  addNodeCell "sync" [ UI.span ## (id' <> "__node-sync-progress")
                               # set text "—"
                     ]
  addNodeCell "system-start-time" [ UI.span ## (id' <> "__node-system-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "start-time"        [ UI.span ## (id' <> "__node-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "uptime"   [ UI.span ## (id' <> "__node-uptime")
                                   # set text "—"
                         ]
  addNodeCell "logs"     [ UI.span ## (id' <> "__node-logs")
                                   #+ ls
                         ]
  addNodeCell "block-replay" [ UI.span ## (id' <> "__node-block-replay")
                                       # set html "0&nbsp;%"
                             ]
  addNodeCell "chunk-validation" [ UI.span ## (id' <> "__node-chunk-validation")
                                           # set text "—"
                                 ]
  addNodeCell "update-ledger-db" [ UI.span ## (id' <> "__node-update-ledger-db")
                                           # set html "0&nbsp;%"
                                 ]
  addNodeCell "peers" [ UI.div #. "buttons has-addons" #+
                          [ UI.button ## (id' <> "__node-peers-num")
                                      #. "button is-static"
                                      # set text "—"
                          , element peersDetailsButton
                          ]
                      , element peersTable
                      ]
  addNodeCell "leadership" [ UI.span ## (id' <> "__node-leadership")
                                     # set text "—"
                           ]
  addNodeCell "forged-blocks" [ UI.span ## (id' <> "__node-forged-blocks")
                                        # set text "—"
                              ]
  addNodeCell "cannot-forge" [ UI.span ## (id' <> "__node-cannot-forge")
                                       # set text "—"
                             ]
  addNodeCell "missed-slots" [ UI.span ## (id' <> "__node-missed-slots")
                                       # set text "—"
                             ]
  addNodeCell "ekg-metrics" [ UI.div #. "buttons has-addons" #+
                                [ UI.button ## (id' <> "__node-ekg-metrics-num")
                                            #. "button is-static"
                                            # set text "—"
                                , element ekgMetricsButton
                                ]
                            , element ekgMetricsWindow
                            ]
 where
  addNodeCellH rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.th #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]
  addNodeCell rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.td #. (unpack anId <> "__column_cell")
                                   #+ cellContent
                           ]

-- | The new node is already connected, so we can display its logging settings.
logsSettings
  :: NonEmpty LoggingParams
  -> String
  -> Bool
  -> UI [UI Element]
logsSettings loggingConfig anId itIsDarkTheme =
  forM (NE.toList loggingConfig) $ \(LoggingParams root mode format) ->
    case mode of
      FileMode -> do
        let logsPathClasses =
              if itIsDarkTheme
                then "tag is-info is-light is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"
                else "tag is-info is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"
            logsFormatClasses =
              if itIsDarkTheme
                then "tag is-warning is-light is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"
                else "tag is-warning is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"
        let pathToSubdir = root </> anId
        copyPath <- image "has-tooltip-multiline has-tooltip-top rt-view-copy-icon" copySVG
                          # set dataTooltip "Click to copy the path to a directory with logs from this node"
        on UI.click copyPath . const $
          copyTextToClipboard pathToSubdir
        return $
          UI.p #+
            [ UI.span #. logsPathClasses
                      # set dataTooltip (pathToSubdir
                                         <> " is the path to a directory with logs from this node")
                      # set text (shortenPath pathToSubdir)
            , element copyPath
            , UI.span #. logsFormatClasses
                      # set dataTooltip "The format log files are written in"
                      # set text (if format == ForHuman then "LOG" else "JSON")
            ]
      JournalMode -> do
        copyId <- image "has-tooltip-multiline has-tooltip-top rt-view-copy-icon" copySVG
                        # set dataTooltip "Click to copy the syslog identifier of this node"
        on UI.click copyId . const $
          copyTextToClipboard anId
        return $
          UI.p #+
            [ UI.span #. ("tag is-info is-light is-rounded mr-3"
                          <> " has-tooltip-multiline has-tooltip-top")
                      # set dataTooltip (anId <> " is the syslog identifier for this node")
                      # set text (shortenPath anId)
            , element copyId
            , UI.span #. ("tag is-warning is-light is-rounded ml-3 "
                          <> "has-tooltip-multiline has-tooltip-top")
                      # set dataTooltip "Logs from this node are written in systemd's journal"
                      # set text "JRNL"
            ]

-- | The node was disconnected, so its column should be deleted.
deleteNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
deleteNodeColumn window (NodeId anId) = do
  let className = anId <> "__column_cell"
  findByClassAndDo window className UI.delete
