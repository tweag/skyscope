{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Posix.Daemonize (daemonize)
import Text.Read (readMaybe)

import qualified Import
import qualified Server

main :: IO ()
main = do
  getArgs >>= \case

    [ "import", path ] -> Import.importer path

    [ "server", port ] -> case readMaybe port of
      Nothing -> error $ "unable to parse port: " <> port
      Just port -> daemonize $ Server.server port

    _ -> do
      putStrLn $ unlines
        [ "usage: skyscope import DATABASE-PATH  # Import Skyframe dump from stdin to DATABASE"
        , "       skyscope server PORT           # Start server process listening on PORT"
        ]
      exitFailure
