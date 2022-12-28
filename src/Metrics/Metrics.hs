{-# LANGUAGE OverloadedStrings #-}
module Metrics.Metrics where

import Data.Text
import Prometheus

type ProjectLabel = Text

data ProjectMetrics = ProjectMetrics
  { projectMetricsProcessedPR  :: Vector ProjectLabel Counter
  , projectMetricsMergedPR     :: Vector ProjectLabel Counter
  , projectMetricsFailedPR     :: Vector ProjectLabel Counter
  , projectMetricsQueueAdded   :: Vector ProjectLabel Counter
  , projectMetricsQueueRemoved :: Vector ProjectLabel Counter
  }

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

incProjectMergedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectMergedPR metrics project =
  withLabel (projectMetricsMergedPR metrics) project incCounter

incProjectFailedPR :: ProjectMetrics -> ProjectLabel -> IO ()
incProjectFailedPR metrics project =
  withLabel (projectMetricsFailedPR metrics) project incCounter
