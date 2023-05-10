{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Import
import qualified Server
import System.Environment (getArgs, unsetEnv)
import System.Exit (exitFailure)
import System.Posix.Daemonize (daemonize)
import Text.Read (readMaybe)

main :: IO ()
main = do
  unsetEnv "LD_LIBRARY_PATH"
  getArgs >>= \case
    ["import-json", path] -> Import.importJson path
    ["import-skyframe", path] -> Import.importSkyframe path
    ["import-targets", path] -> Import.importTargets path
    ["import-actions", path] -> Import.importActions path
    ["server", port] -> case readMaybe port of
      Nothing -> error $ "unable to parse port: " <> port
      Just port -> daemonize $ Server.server port
    _ -> putStrLn "invalid args" *> exitFailure
