{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MonadLoggerEffect where

import Control.Monad.Logger(ToLogStr, Loc, LogSource, LogLevel, MonadLogger (..))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)

data MonadLoggerEffect :: Effect where
    MonadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> MonadLoggerEffect m ()

type instance DispatchOf MonadLoggerEffect = 'Dynamic

instance MonadLoggerEffect :> es => MonadLogger (Eff es) where
  monadLoggerLog loc logSource logLevel msg = send $ MonadLoggerLog loc logSource logLevel msg
