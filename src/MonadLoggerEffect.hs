{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MonadLoggerEffect where

import Control.Monad.Logger (Loc, LogLevel, LogSource, MonadLogger (..), ToLogStr (toLogStr),
                             defaultOutput)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import System.IO (stdout)

data MonadLoggerEffect :: Effect where
    MonadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> MonadLoggerEffect m ()

type instance DispatchOf MonadLoggerEffect = 'Dynamic

instance MonadLoggerEffect :> es => MonadLogger (Eff es) where
  monadLoggerLog loc logSource logLevel msg = send $ MonadLoggerLog loc logSource logLevel msg

-- | Run the logger such that everything is logged to stdout
runLoggerStdout :: IOE :> es => Eff (MonadLoggerEffect : es) a -> Eff es a
runLoggerStdout = interpret $ \_ -> \case
  MonadLoggerLog loc logSource logLevel msg ->
    liftIO $ defaultOutput stdout loc logSource logLevel $ toLogStr msg
