{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Metrics.Metrics
(
  MetricsOperation,
  MetricsOperationFree (..),
  ProjectMetrics (..),
  runMetrics,
  runLoggingMonitorT,
  increaseMergedPRTotal,
  registerProjectMetrics
  )
where

import Data.Text
import Prometheus
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Free (Free)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Free.Ap (liftF)

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR     :: Vector ProjectLabel Counter
  }

data MetricsOperationFree a
  = MergeBranch a
  deriving (Functor)

type MetricsOperation = Free MetricsOperationFree

newtype LoggingMonitorT m a = LoggingMonitorT { runLoggingMonitorT :: LoggingT m a }
                              deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadIO m => MonadMonitor (LoggingMonitorT m) where
  doIO = liftIO

increaseMergedPRTotal :: MetricsOperation ()
increaseMergedPRTotal = liftF $ MergeBranch ()

runMetrics
  :: (MonadMonitor m, MonadIO m)
  => ProjectMetrics
  -> ProjectLabel
  -> MetricsOperationFree a
  -> m a
runMetrics metrics label operation =
  case operation of
    MergeBranch cont -> cont <$
      incProjectMergedPR metrics label

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics = ProjectMetrics
  <$> register (vector "project" (counter (Info "hoff_project_merged_pull_requests"
                                                 "Number of merged pull requests")))

incProjectMergedPR :: (MonadMonitor m, MonadIO m) => ProjectMetrics -> ProjectLabel -> m ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter
