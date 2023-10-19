{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common (getDataDirectory, getSkyscopeEnv)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, tryJust)
import Control.Monad (guard, when)
import Data.Aeson (decode, encode)
import Data.Bifunctor (first, second)
import Data.Foldable (traverse_)
import Data.Functor (void, (<&>))
import Data.List (isPrefixOf, stripPrefix)
import qualified Import
import Network.HTTP.Client (Request (..), RequestBody (..), defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody)
import qualified Server
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, setEnv)
import System.FilePath.Posix (takeBaseName)
import System.IO (stdin)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (emptyTempFile)
import System.Posix.Daemonize (daemonize)
import System.Posix.IO (FdOption (..), closeFd, dup, dupTo, handleToFd, setFdOption, stdInput)
import System.Posix.Process (forkProcess)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), StdStream (..), proc, readProcess, withCreateProcess)
import Text.Read (readMaybe)

main :: IO ()
main = do
  restartServer
  getArgs >>= \case
    ["server"] -> pure ()
    "import" : args -> importWorkspace args
    "import-graphviz" : args -> importGraphviz args
    _ -> usageError

usageError :: a
usageError =
  error $
    "usage: skyscope server|import [--query=EXPR|--no-query] [--aquery=EXPR|--no-query]\n"
      <> "                 skyscope import-graphviz [TAG]"

importNew :: Maybe String -> (FilePath -> IO ()) -> IO ()
importNew workspace populateDatabase = do
  workspace <- case workspace of
    Just workspace -> pure workspace
    Nothing -> do
      workspace <- getBazelWorkspace
      setEnv "SKYSCOPE_WORKSPACE" workspace
      setEnv "SKYSCOPE_OUTPUT_BASE" =<< getBazelOutputBase
      pure workspace

  -- Create a new sqlite database to import into.
  let dbTemplate = takeBaseName workspace <> ".sqlite"
  importsDir <- getDataDirectory <&> (<> "/imports")
  createDirectoryIfMissing True importsDir
  dbPath <- emptyTempFile importsDir dbTemplate

  populateDatabase dbPath

  putStrLn "import complete, notifying server"
  notifyServer workspace dbPath

importGraphviz :: [String] -> IO ()
importGraphviz args = importNew tag $ Import.importGraphviz stdin
  where
    tag = Just $ case args of
      [] -> "graphviz-import"
      [tag] -> tag
      _ -> usageError

importWorkspace :: [String] -> IO ()
importWorkspace args = importNew Nothing $ \dbPath -> do
  let withBazel args f = do
        logCommand "bazel" args
        withCreateProcess
          (proc "bazel" args)
            { std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = Inherit
            }
          $ \_ (Just bazelStdout) _ _ -> f bazelStdout dbPath

  let (queryExpr, aqueryExpr) = parseImportArgs args
  when (aqueryExpr /= "") $ do
    putStrLn "importing extra context for actions (pass --no-aquery to skip this step)"
    (withBazel ["aquery", aqueryExpr] Import.importActions)
  when (queryExpr /= "") $ do
    putStrLn "importing extra context for targets (pass --no-query to skip this step)"
    (withBazel ["query", queryExpr, "--output", "build"] Import.importTargets)

  -- Skyframe dump option depends on Bazel version.
  dumpSkyframeOpt <-
    getBazelVersion >>= \case
      Just version
        | or
            [ "3." `isPrefixOf` version,
              "4." `isPrefixOf` version,
              "5." `isPrefixOf` version
            ] ->
          "detailed" <$ setEnv "SKYSCOPE_LEGACY_BAZEL" "1"
        | otherwise -> pure "deps"
      Nothing -> error "unable to determine bazel version"
  withStdinFrom "bazel" ["dump", "--skyframe=" <> dumpSkyframeOpt] (Import.importSkyframe dbPath)

parseImportArgs :: [String] -> (String, String)
parseImportArgs = \case
  arg : args
    | arg == "--no-query" -> first (const "") (parseImportArgs args)
    | arg == "--no-aquery" -> second (const "") (parseImportArgs args)
    | otherwise -> case (stripPrefix "--query=" arg, stripPrefix "--aquery=" arg) of
      (Just queryExpr, _) -> first (const queryExpr) (parseImportArgs args)
      (_, Just aqueryExpr) -> second (const aqueryExpr) (parseImportArgs args)
      _ -> error $ "invalid arg: " <> arg
  [] -> ("", "")

notifyServer :: String -> FilePath -> IO ()
notifyServer workspace dbPath = do
  serverPort <- getServerPort
  httpManager <- newManager defaultManagerSettings
  let urlBase = "http://localhost:" <> show serverPort

  request <- parseRequest urlBase
  response <-
    httpLbs
      request
        { requestBody = RequestBodyLBS $ encode (workspace, dbPath),
          method = "POST"
        }
      httpManager

  case decode $ responseBody response of
    Nothing -> error "unexpected result from server"
    Just Server.Import {..} ->
      let url = urlBase <> "/" <> show importId
       in putStrLn $ "\nOpen this link in your browser:\n  \x1b[1;36m" <> url <> "\x1b[0m\n"

restartServer :: IO ()
restartServer = do
  traverse_ attemptTerminate =<< getPidFromFile
  void $ forkProcess $ daemonize . Server.server =<< getServerPort
  where
    attemptTerminate :: ProcessID -> IO ()
    attemptTerminate pid = void $
      tryJust (guard . isDoesNotExistError) $ do
        putStrLn $ "restarting server (previous pid " <> show pid <> ")"
        signalProcess sigTERM pid
        threadDelay 1_000_000

    getPidFromFile :: IO (Maybe ProcessID)
    getPidFromFile = do
      pidFile <- getDataDirectory <&> (<> "/server.pid")
      tryJust (guard . isDoesNotExistError) (readMaybe <$> readFile pidFile) >>= \case
        Right Nothing -> error $ "failed to parse pid in " <> pidFile
        Right (Just pid) -> pure $ Just pid
        Left () -> pure Nothing

getServerPort :: IO Int
getServerPort =
  getSkyscopeEnv "PORT" >>= \case
    Just string -> case readMaybe string of
      Nothing -> error $ "failed to parse SKYSCOPE_PORT: " <> string
      Just port -> pure port
    Nothing -> pure 28581

withStdinFrom :: String -> [String] -> IO a -> IO a
withStdinFrom command args action = do
  logCommand command args
  withCreateProcess
    (proc command args)
      { std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = Inherit
      }
    $ \_ (Just procStdout) _ _ -> do
      procStdout <- handleToFd procStdout
      setFdOption procStdout NonBlockingRead False
      let redirectStdin = dup stdInput <* dupTo procStdout stdInput
          restoreStdin originalStdin = closeFd procStdout *> dupTo originalStdin stdInput
      bracket redirectStdin restoreStdin $ const action

logCommand :: String -> [String] -> IO ()
logCommand command args = putStrLn $ "\x1b[1;37m" <> command <> " " <> unwords args <> "\x1b[0m"

getBazelOutputBase :: IO FilePath
getBazelOutputBase =
  lines <$> readProcess "bazel" ["info", "output_base"] "" <&> \case
    outputBase : _ -> outputBase
    _ -> error "failed to get output_base"

getBazelWorkspace :: IO FilePath
getBazelWorkspace =
  lines <$> readProcess "bazel" ["info", "workspace"] "" <&> \case
    workspace : _ -> workspace
    _ -> error "failed to get workspace"

getBazelVersion :: IO (Maybe String)
getBazelVersion = do
  let find = \case
        [] -> Nothing
        line : remaining -> case stripPrefix "Build label: " line of
          Just version -> pure version
          Nothing -> find remaining
  lines <$> readProcess "bazel" ["version"] "" <&> find
