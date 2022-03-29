{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.CSS.Own
  ( ownCSS
  , chartGridDark
  , chartGridLight
  , chartTextDark
  , chartTextLight
  ) where

import           Data.String.QQ

-- | To avoid run-time dependency from the static content, embed own CSS in the page's header.
ownCSS :: String
ownCSS = [s|
html {
  height: 100%;
}

body {
  height: 100%;
}

#wrapper {
  min-height: 100%;
  width: 100%;
  position: relative;
  padding-bottom: 62px;
  box-sizing: border-box;
}

.footer {
  height: 62px;
  bottom: 0;
  position: absolute;
  width: 100%;
}

code {
  color: #1d359f;
  padding: 0.11em 0.2em 0.11em;
  border-radius: 3px;
}

span[data-tooltip] {
  border-bottom: none !important;
}

.field.is-grouped {
  display: inline-flex !important;
}

.rt-view-no-nodes-info {
  max-width: 600px !important;
  margin-top: 50px;
}

.rt-view-chart-area {
  width: 100% !important;
}

.rt-view-main-table-description {
  min-width: 380px;
}

.rt-view-node-chart-label svg {
  width: 42px;
  height: 15px;
  margin-right: 3px;
}

.rt-view-peer-modal {
  width: 45%;
}

@media only screen and (max-width: 1216px) {
  .rt-view-peer-modal {
    width: 60%;
  }
}

@media only screen and (max-width: 1024px) {
  .rt-view-peer-modal {
    width: 70%;
  }
}

@media only screen and (max-width: 769px) {
  .rt-view-peer-modal {
    width: 80%;
  }
}

/* Dark Theme */

.dark {
  font-family: sans-serif;
  font-size: 22px;
  background-color: #131325;
  min-height: 100%;
}

.dark #wrapper {
  background-color: #131325;
}

.dark .pageloader {
  background: #2c2b3b !important;
  opacity: 0.95 !important;
}

.dark .rt-view-href {
  color: #607bf7;
}

.dark .rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

.dark .rt-view-href-icon svg {
  width: 12px;
  margin-left: 5px;
  margin-bottom: 4px;
  color: #607bf7;
}

.dark .rt-view-top-bar {
  background-color: #282841;
  color: whitesmoke;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #555;
}

.dark .rt-view-cardano-logo svg {
  width: 48px;
  color: whitesmoke;
  margin-left: 5px;
}

.dark .rt-view-name {
  color: whitesmoke;
  margin-left: 17px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.dark .rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-theme-icon svg {
  width: 23px;
  padding-top: 2px;
  margin-right: 13px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-copy-icon svg {
  width: 20px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-logs-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
}

.dark .rt-view-overview-icon svg {
  width: 18px;
  margin-right: 12px;
  color: #0cc9cb;
}

.dark .rt-view-no-nodes-icon svg {
  width: 70px;
  margin-top: 60px;
  margin-bottom: 40px;
  color: #677deb;
}

.dark .rt-view-no-nodes-message {
  font-size: 23px;
  color: whitesmoke;
}

.dark .rt-view-charts-container {
  padding-bottom: 15px;
}

.dark .rt-view-chart-container {
  background-color: #2c2b3b;
  padding-top: 10px;
  padding-bottom: 20px;
  padding-left: 20px;
  padding-right: 20px;
  margin-left: 16px;
  margin-right: 16px;
  margin-bottom: 50px;
  border: 1px solid #444;
  border-radius: 6px;
}

.dark .rt-view-chart-name {
  color: whitesmoke;
}

.dark .rt-view-about-title {
  color: whitesmoke;
}

.dark .rt-view-about-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-about-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-peer-title {
  color: whitesmoke;
}

.dark .rt-view-peer-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-peer-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-main-table {
  background-color: #131325;
  color: whitesmoke;
}

.dark .rt-view-main-table td {
  padding-top: 17px;
  padding-bottom: 17px;
  border-bottom: 0px solid #444;
}

.dark .rt-view-main-table th {
  color: whitesmoke;
  border-bottom: 2px solid #888;
  vertical-align: middle;
}

.dark .rt-view-peer-table {
  background-color: #131325;
  color: whitesmoke;
}

.dark .rt-view-peer-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.dark .rt-view-peer-table th {
  color: whitesmoke;
  border-bottom: 2px solid #888;
  vertical-align: middle;
}

.dark .rt-view-chart-group-title {
  color: whitesmoke;
  font-weight: bold;
  margin-left: 15px;
  font-size: 115%;
}

.dark .rt-view-show-hide-chart-group svg {
  width: 21px;
  margin-left: 17px;
  margin-right: 3px;
  color: #0cc9cb;
}

.dark .rt-view-chart-icon svg {
  width: 19px;
  margin-right: 11px;
  color: #0cc9cb;
}

.dark .rt-view-footer {
  background-color: #282841;
  color: #999;
  padding-top: 16px;
  padding-bottom: 12px;
  border-top: 1px solid #555;
  font-size: 80%;
}

.dark .rt-view-footer-github svg {
  width: 23px;
  margin-top: 1px;
  color: #0cc9cb;
}

.dark .rt-view-percent-done {
  color: #07e949;
}

.dark .rt-view-what-icon svg {
  width: 18px;
  margin-left: 12px;
  color: #999;
}

.dark .rt-view-epoch-end svg {
  width: 16px;
  margin-left: 20px;
  margin-right: 5px;
  color: #0cc9cb;
}

/**** Light Theme ****/

.light {
  font-family: sans-serif;
  font-size: 22px;
  background-color: #f5f5f5;
  min-height: 100%;
}

.light #wrapper {
  background-color: #f5f5f5;
}

.light .pageloader {
  background: #2c2b3b !important;
  opacity: 0.95 !important;
}

.light .rt-view-href {
  color: #264af0;
}

.light .rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

.light .rt-view-href-icon svg {
  width: 12px;
  margin-left: 5px;
  margin-bottom: 4px;
  color: #264af0;
}

.light .rt-view-top-bar {
  background-color: #efefef;
  color: #131325;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #dbdbdb;
}

.light .rt-view-cardano-logo svg {
  width: 48px;
  color: #0033ad;
  margin-left: 5px;
}

.light .rt-view-name {
  color: #0033ad;
  margin-left: 17px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.light .rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: #0033ad;
  cursor: pointer;
}

.light .rt-view-theme-icon svg {
  width: 23px;
  padding-top: 2px;
  margin-right: 13px;
  color: #0033ad;
  cursor: pointer;
}

.light .rt-view-copy-icon svg {
  width: 20px;
  color: #444;
  cursor: pointer;
}

.light .rt-view-logs-icon svg {
  width: 23px;
  padding-top: 2px;
  color: #0033ad;
}

.light .rt-view-overview-icon svg {
  width: 18px;
  margin-right: 12px;
  color: #038b8c;
}

.light .rt-view-no-nodes-icon svg {
  width: 70px;
  margin-top: 60px;
  margin-bottom: 40px;
  color: #0033ad;
}

.light .rt-view-no-nodes-message {
  font-size: 23px;
  color: #0033ad;
}

.light .rt-view-charts-container {
  padding-bottom: 15px;
}

.light .rt-view-chart-container {
  background-color: #eeeeee;
  padding-top: 10px;
  padding-bottom: 20px;
  padding-left: 20px;
  padding-right: 20px;
  margin-left: 16px;
  margin-right: 16px;
  margin-bottom: 50px;
  border: 1px solid #dddddd;
  border-radius: 6px;
}

.light .rt-view-chart-name {
  color: #444;
}

.light .rt-view-about-title {
  color: #444;
}

.light .rt-view-about-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-about-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-peer-title {
  color: #444;
}

.light .rt-view-peer-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-peer-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-main-table {
  background-color: #f5f5f5;
  color: #444;
}

.light .rt-view-main-table td {
  padding-top: 17px;
  padding-bottom: 17px;
  border-bottom: 0px solid #444;
}

.light .rt-view-main-table th {
  color: #444;
  border-bottom: 2px solid #cfcfcf;
  vertical-align: middle;
}

.light .rt-view-peer-table {
  background-color: #eaeaea;
  color: #444;
}

.light .rt-view-peer-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.light .rt-view-peer-table th {
  color: #444;
  border-bottom: 2px solid #cfcfcf;
  vertical-align: middle;
}

.light .rt-view-chart-group-title {
  color: #444;
  font-weight: bold;
  margin-left: 15px;
  font-size: 110%;
}

.light .rt-view-show-hide-chart-group svg {
  width: 21px;
  margin-left: 17px;
  margin-right: 3px;
  color: #038b8c;
}

.light .rt-view-chart-icon svg {
  width: 19px;
  margin-right: 11px;
  color: #038b8c;
}

.light .rt-view-footer {
  background-color: #efefef;
  color: #777;
  padding-top: 16px;
  padding-bottom: 12px;
  border-top: 1px solid #dbdbdb;
  font-size: 80%;
}

.light .rt-view-footer-github svg {
  width: 23px;
  margin-top: 1px;
  color: #038b8c;
}

.light .rt-view-percent-done {
  color: #048b04;
}

.light .rt-view-what-icon svg {
  width: 18px;
  margin-left: 12px;
  color: #9a9a9a;
}

.light .rt-view-epoch-end svg {
  width: 16px;
  margin-left: 20px;
  margin-right: 5px;
  color: #038b8c;
}
|]

chartTextLight
  , chartTextDark
  , chartGridDark
  , chartGridLight :: String
chartGridDark  = "#ccc"
chartGridLight = "#555"
chartTextDark  = "#555"
chartTextLight = "#ddd"
