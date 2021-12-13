{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Time where
import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)


newtype TimeOperationFree a 
  = GetDateTime (UTCTime -> a)
  deriving (Functor)

type TimeOperation = Free TimeOperationFree

getDateTime :: TimeOperation UTCTime
getDateTime = liftF $ GetDateTime id

runTime :: MonadIO m => TimeOperationFree a -> m a
runTime (GetDateTime cont) = cont <$> liftIO getCurrentTime 