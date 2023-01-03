{-# LANGUAGE OverloadedStrings #-}

module Metrics.Server
(
  MetricsServerConfig (..),
  serverConfig,
  runMetricsServer
)
where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Prometheus as PrometheusWai
import Data.Function ((&))

data MetricsServerConfig = MetricsServerConfig
  { metricsConfigHost :: Warp.HostPreference
  , metricsConfigPort :: Warp.Port
  }

serverConfig :: MetricsServerConfig -> Warp.Settings
serverConfig config = Warp.defaultSettings
  & Warp.setHost (metricsConfigHost config)
  & Warp.setPort (metricsConfigPort config)

runMetricsServer :: MetricsServerConfig -> IO ()
runMetricsServer config = Warp.runSettings (serverConfig config) PrometheusWai.metricsApp
