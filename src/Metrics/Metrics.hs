{-# LANGUAGE OverloadedStrings #-}
module Metrics.Metrics where

import Prometheus

data ProjectMetrics = ProjectMetrics
  { projectMetricsProcessedPR  :: Counter
  , projectMetricsMergedPR     :: Counter
  , projectMetricsFailedPR     :: Counter
  , projectMetricsQueueAdded   :: Counter
  , projectMetricsQueueRemoved :: Counter
  }

registerProjectMetrics :: IO ProjectMetrics
registerProjectMetrics = ProjectMetrics
  <$> register (counter (Info "hoff_project_processed_pull_requests"
                              "Number of processed pull requests"))
  <*> register (counter (Info "hoff_project_merged_pull_requests"
                              "Number of merged pull requests"))
  <*> register (counter (Info "hoff_project_failed_pull_requests"
                              "Number of failed pull requests"))
  <*> register (counter (Info "hoff_project_queue_added" "Number of items added to the queue"))
  <*> register (counter (Info "hoff_project_queue_added" "Number of items removed from the queue"))
