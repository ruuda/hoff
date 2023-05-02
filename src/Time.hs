{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Time (getDateTime, sleepMicros, runTime, TimeOperation (..)) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data TimeOperation :: Effect where
  GetDateTime :: TimeOperation m UTCTime
  SleepMicros :: Int -> TimeOperation m ()

type instance DispatchOf TimeOperation = 'Dynamic

getDateTime :: TimeOperation :> es => Eff es UTCTime
getDateTime = send GetDateTime

sleepMicros :: TimeOperation :> es => Int -> Eff es ()
sleepMicros micros = send $ SleepMicros micros

runTime :: IOE :> es => Eff (TimeOperation : es) a -> Eff es a
runTime = interpret $ \_ -> \case
  GetDateTime -> liftIO getCurrentTime
  SleepMicros micros -> void $ liftIO (threadDelay micros)
