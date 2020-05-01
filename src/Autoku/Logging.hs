{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Autoku.Logging where


import           Prelude                  hiding (log)

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Char
import           Data.Has
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           System.Exit
import           Text.Printf



-- log level

data LogLevel = Debug
              | Info
              | Warning
              | Error
              deriving (Show, Eq, Ord)



-- logger effect

type Logger m = LogLevel -> String -> m ()

class Monad m => MonadLogger m where
  log :: Logger m


logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error
logFatal   = logError >=> const (throw $ ExitFailure 1)



-- loggers

ioLogger :: (MonadIO m, MonadReader r m, Has LogLevel r) => Logger m
ioLogger level msg = do
  minLevel <- asks getter
  when (level >= minLevel) $ liftIO $ do
    time <- take 19 . iso8601Show <$> getCurrentTime
    printf "[%s] %s: %s\n" time (map toUpper $ show level) msg

writerLogger :: (MonadWriter [String] m, MonadReader r m, Has LogLevel r) => Logger m
writerLogger level msg = do
  minLevel <- asks getter
  when (level >= minLevel) $
    tell $ pure $ printf "%s: %s\n" (map toUpper $ show level) msg

nullLogger :: Monad m => Logger m
nullLogger _ _ = pure ()
