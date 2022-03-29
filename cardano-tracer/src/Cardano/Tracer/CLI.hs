module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Options.Applicative

-- | CLI parameters required for the tracer.
data TracerParams = TracerParams
  { tracerConfig :: !FilePath
  , checkMode    :: !Bool
  }

-- | Parse CLI parameters for the tracer.
parseTracerParams :: Parser TracerParams
parseTracerParams = TracerParams
  <$> strOption
        (    long "config"
          <> short 'c'
          <> metavar "FILEPATH"
          <> help "Configuration file for cardano-tracer"
          <> completer (bashCompleter "file")
        )
  <*> flag
        False
        True
        (    long "check-mode"
          <> help "Run in check mode: shows all accepted stuff"
        )
