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
  runNoMonitorT,
  increaseMergedPRTotal,
  updateTrainSizeGauge,
  registerGHCMetrics,
  registerProjectMetrics
  )
where

import Data.Text
import Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Free (Free)
import Control.Monad.Logger (LoggingT, MonadLogger, NoLoggingT)
import Control.Monad.Free.Ap (liftF)
import Control.Monad (void)

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsMergedPR :: Vector ProjectLabel Counter
  , projectMetricsMergeTrainSize :: Vector ProjectLabel Gauge
  }

data MetricsOperationFree a
  = MergeBranch a
  | UpdateTrainSize Int a
  deriving (Functor)

type MetricsOperation = Free MetricsOperationFree

newtype LoggingMonitorT m a = LoggingMonitorT { runLoggingMonitorT :: LoggingT m a }
                              deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

newtype NoMonitorT m a = NoMonitorT { runNoMonitorT :: NoLoggingT m a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadIO m => MonadMonitor (LoggingMonitorT m) where
  doIO = liftIO


instance MonadIO m => MonadMonitor (NoMonitorT m) where
  doIO _ = return ()

increaseMergedPRTotal :: MetricsOperation ()
increaseMergedPRTotal = liftF $ MergeBranch ()

updateTrainSizeGauge :: Int -> MetricsOperation ()
updateTrainSizeGauge n = liftF $ UpdateTrainSize n ()

runMetrics
  :: (MonadMonitor m, MonadIO m)
  => ProjectMetrics
  -> ProjectLabel
  -> MetricsOperationFree a
  -> m a
runMetrics metrics label operation =
  case operation of
    UpdateTrainSize n cont -> cont <$
      setProjectMetricMergeTrainSize metrics label n
    MergeBranch cont -> cont <$
      incProjectMergedPR metrics label

registerGHCMetrics :: IO ()
registerGHCMetrics = void $ register ghcMetrics

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics = ProjectMetrics
  <$> register (vector "project" (counter (Info "hoff_project_merged_pull_requests"
                                                 "Number of merged pull requests")))
  <*> register (vector "project" (gauge (Info "hoff_project_merge_train_size"
                                                "Number of pull requests currently in the queue (merge train)")))

incProjectMergedPR :: (MonadMonitor m, MonadIO m) => ProjectMetrics -> ProjectLabel -> m ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter

setProjectMetricMergeTrainSize :: (MonadMonitor m, MonadIO m) => ProjectMetrics -> ProjectLabel -> Int -> m ()
setProjectMetricMergeTrainSize metrics project n =
  withLabel (projectMetricsMergeTrainSize metrics) project (\g -> setGauge g (fromIntegral n))
