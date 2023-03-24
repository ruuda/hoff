{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Time (getDateTime, sleepMicros, runTime, TimeOperationFree (..), TimeOperation) where

import Control.Concurrent (threadDelay)
import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)

data TimeOperationFree a
  = GetDateTime (UTCTime -> a)
  | SleepMicros Int a
  deriving (Functor)

type TimeOperation = Free TimeOperationFree

getDateTime :: TimeOperation UTCTime
getDateTime = liftF $ GetDateTime id

sleepMicros :: Int -> TimeOperation ()
sleepMicros micros = liftF $ SleepMicros micros ()

runTime :: MonadIO m => TimeOperationFree a -> m a
runTime (GetDateTime cont) = cont <$> liftIO getCurrentTime
runTime (SleepMicros micros cont) = cont <$ liftIO (threadDelay micros)
