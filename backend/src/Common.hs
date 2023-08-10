{-# LANGUAGE LambdaCase #-}

module Common where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clock
import Debug.Trace (trace)
import System.Environment (lookupEnv)

type Memoize e a = ReaderT e IO a

memoize ::
  (Ord k, Show k {- , Show a -}) =>
  String ->
  (e -> TVar (Map k a)) ->
  (k -> Memoize e a) ->
  k ->
  Memoize e a
memoize label memo action key = do
  memo <- asks memo
  liftIO (Map.lookup key <$> readTVarIO memo) >>= \case
    Just value -> liftIO $ value <$ log 30 "hit" ""
    Nothing ->
      action key >>= \value -> liftIO $ do
        atomically $ modifyTVar memo $ Map.insert key value
        value <$ log 31 "miss\x1b[0m" (ansi 37 "  →   " <> {-show value-} "<value>")
  where
    ansi :: Integer -> String -> String
    ansi n s = "\x1b[" <> show n <> "m" <> s <> "\x1b[0m"

    log :: Integer -> String -> String -> IO ()
    log n outcome value =
      if enableLogging
        then
          putStrLn $
            ansi n $
              "memo cache "
                <> outcome
                <> " for "
                <> label
                <> ": "
                <> show key
                <> value
        else pure ()

    enableLogging :: Bool
    enableLogging = False

timed :: MonadIO m => String -> m a -> m a
timed label action = do
  startTime <- liftIO $ Clock.getCurrentTime
  result <- action
  endTime <- liftIO $ Clock.getCurrentTime
  liftIO $
    putStrLn $
      label <> " took "
        <> show
          ( Clock.nominalDiffTimeToSeconds $
              Clock.diffUTCTime endTime startTime
          )
        <> " seconds"
  pure result

traceValue :: Show a => String -> a -> a
traceValue label x = trace (label <> " = " <> show x) x

getSkyscopeEnv :: String -> IO (Maybe String)
getSkyscopeEnv name = lookupEnv $ "SKYSCOPE_" <> name
