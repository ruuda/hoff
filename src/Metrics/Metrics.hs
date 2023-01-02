{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Metrics.Metrics where

import Data.Text
import Prometheus
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Free (Free)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Free.Ap (liftF)

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsProcessedPR  :: Vector ProjectLabel Counter
  , projectMetricsMergedPR     :: Vector ProjectLabel Counter
  , projectMetricsFailedPR     :: Vector ProjectLabel Counter
  , projectMetricsQueueAdded   :: Vector ProjectLabel Counter
  , projectMetricsQueueRemoved :: Vector ProjectLabel Counter
  }

data MetricsOperationFree a
  = MergeBranch a
  | MergeFailure a
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
    MergeFailure cont -> cont <$
      incProjectFailedPR metrics label

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics = ProjectMetrics
  <$> register (vector "project" (counter (Info "hoff_project_processed_pull_requests"
                                                 "Number of processed pull requests")))
  <*> register (vector "project" (counter (Info "hoff_project_merged_pull_requests"
                                                 "Number of merged pull requests")))
  <*> register (vector "project" (counter (Info "hoff_project_failed_pull_requests"
                                                 "Number of failed pull requests")))
  <*> register (vector "project" (counter (Info "hoff_project_queue_added" "Number of items added to the queue")))
  <*> register (vector "project" (counter (Info "hoff_project_queue_added" "Number of items removed from the queue")))

incProjectProcessedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectProcessedPR metrics project =
  withLabel (projectMetricsProcessedPR metrics) project incCounter

incProjectMergedPR :: (MonadMonitor m, MonadIO m) => ProjectMetrics -> ProjectLabel -> m ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter

incProjectFailedPR :: (MonadMonitor m, MonadIO m) => ProjectMetrics -> ProjectLabel -> m ()
incProjectFailedPR metrics project =
  withLabel (projectMetricsFailedPR metrics) project incCounter
