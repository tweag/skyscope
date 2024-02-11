{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Common (getDataDirectory, getSkyscopeEnv)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, tryJust)
import Control.Monad (guard, when)
import Data.Aeson (decode, encode)
import Data.Foldable (asum, traverse_)
import Data.Functor (void, (<&>))
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
import Data.UUID (UUID)
import qualified Import
import Network.HTTP.Client (Request (..), RequestBody (..), defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (Status (..))
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

restartServer :: IO ()
restartServer = do
  traverse_ attemptTerminate =<< getPidFromFile
  void $ forkProcess $ daemonize . Server.server =<< getServerPort
  threadDelay 500_000
  where
    attemptTerminate :: ProcessID -> IO ()
    attemptTerminate pid = void $
      tryJust (guard . isDoesNotExistError) $ do
        putStrLn $ "restarting server (previous pid " <> show pid <> ")"
        signalProcess sigTERM pid
        threadDelay 500_000

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

usageError :: a
usageError =
  error $
    unlines
      [ "usage:",
        "\x1b[1;37m",
        "skyscope server",
        "         import [--query=EXPR] [--aquery=EXPR] [--existing=UUID]",
        "         import-graphviz [LABEL]",
        "\x1b[0m"
      ]

importGraphviz :: [String] -> IO ()
importGraphviz args = importNew label $ Import.importGraphviz stdin
  where
    label = Just $ case args of
      [] -> "graphviz-import"
      [label] -> label
      _ -> usageError

importWorkspace :: [String] -> IO ()
importWorkspace args = do
  let ImportArgs {..} = parseImportArgs args
      withDbPath = case existingImport of
        Just importId -> importExisting importId
        Nothing -> importNew Nothing

  withDbPath $ \dbPath -> do
    case existingImport of
      Nothing -> pure ()
      Just _ ->
        if queryExpr == "" && aqueryExpr == ""
          then error $ "provide a query expression to be imported into " <> dbPath
          else pure ()

    let withBazel args f = do
          bazel <- getBazelPath
          logCommand bazel args
          withCreateProcess
            (proc bazel args)
              { std_in = CreatePipe,
                std_out = CreatePipe,
                std_err = Inherit
              }
            $ \_ (Just bazelStdout) _ _ -> f bazelStdout dbPath

    when (aqueryExpr /= "") $ do
      putStrLn "importing extra context for actions"
      (withBazel ["aquery", aqueryExpr] Import.importActions)

    when (queryExpr /= "") $ do
      putStrLn "importing extra context for targets"
      (withBazel ["query", queryExpr, "--output", "build"] Import.importTargets)

    when (isNothing existingImport) $ do
      dumpSkyframeOpt <-
        getBazelVersion >>= \case
          Just version
            | or -- Skyframe dump option depends on Bazel version.
                [ "3." `isPrefixOf` version,
                  "4." `isPrefixOf` version,
                  "5." `isPrefixOf` version
                ] ->
              "detailed" <$ setEnv "SKYSCOPE_LEGACY_BAZEL" "1"
            | otherwise -> pure "deps"
          Nothing -> "deps" <$ putStrLn "unable to determine bazel version, assuming latest"

      bazel <- getBazelPath
      withStdinFrom bazel ["dump", "--skyframe=" <> dumpSkyframeOpt] (Import.importSkyframe dbPath)

data ImportArgs = ImportArgs
  { queryExpr :: String,
    aqueryExpr :: String,
    existingImport :: Maybe UUID
  }

instance Semigroup ImportArgs where
  lhs <> rhs =
    ImportArgs
      { queryExpr = if queryExpr lhs /= "" then queryExpr lhs else queryExpr rhs,
        aqueryExpr = if aqueryExpr lhs /= "" then aqueryExpr lhs else aqueryExpr rhs,
        existingImport = existingImport lhs <|> existingImport rhs
      }

instance Monoid ImportArgs where
  mempty = ImportArgs "" "" Nothing

parseImportArgs :: [String] -> ImportArgs
parseImportArgs = \case
  arg : args -> case asum <$> sequenceA opts $ arg of
    Just ("--query=", expr) -> ImportArgs expr "" Nothing <> parseImportArgs args
    Just ("--aquery=", expr) -> ImportArgs "" expr Nothing <> parseImportArgs args
    Just ("--existing=", uuid) -> case readMaybe uuid of
      Just uuid -> ImportArgs "" "" (Just uuid) <> parseImportArgs args
      Nothing -> error $ "failed to parse existing uuid: " <> uuid
    Just _ -> error "parser returned unexpected option"
    Nothing -> usageError
  [] -> mempty
  where
    opts = [maybeOpt "--query=", maybeOpt "--aquery=", maybeOpt "--existing="]
    maybeOpt :: String -> String -> Maybe (String, String)
    maybeOpt prefix = fmap (prefix,) . stripPrefix prefix

importExisting :: UUID -> (FilePath -> IO ()) -> IO ()
importExisting importId populateDatabase = do
  Server.Import {..} <- queryServer importId
  _ <- getBazelWorkspace
  populateDatabase importPath

queryServer :: UUID -> IO Server.Import
queryServer importId = do
  serverPort <- getServerPort
  httpManager <- newManager defaultManagerSettings

  request <- parseRequest $ "http://localhost:" <> show serverPort <> "/" <> show importId <> "/metadata"
  response <- httpLbs request {method = "GET"} httpManager

  case responseStatus response of
    Status 404 _ -> error $ "import not found: " <> show importId
    Status 200 _ -> case decode $ responseBody response of
      Nothing -> error "failed to decode metadata"
      Just i -> pure i
    status -> error $ "unexpected result from server: " <> show status

importNew :: Maybe String -> (FilePath -> IO ()) -> IO ()
importNew workspace populateDatabase = do
  workspace <- case workspace of
    Just workspace -> pure workspace
    Nothing -> getBazelWorkspace

  -- Create a new sqlite database to import into.
  let dbTemplate = takeBaseName workspace <> ".sqlite"
  importsDir <- getDataDirectory <&> (<> "/imports")
  createDirectoryIfMissing True importsDir
  dbPath <- emptyTempFile importsDir dbTemplate

  populateDatabase dbPath

  putStrLn "import complete, notifying server"
  notifyServer workspace dbPath

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

getBazelWorkspace :: IO FilePath
getBazelWorkspace = do
  bazel <- getBazelPath
  workspace <-
    lines <$> readProcess bazel ["info", "workspace"] "" <&> \case
      workspace : _ -> workspace
      _ -> error "failed to get workspace"
  setEnv "SKYSCOPE_WORKSPACE" workspace
  setEnv "SKYSCOPE_OUTPUT_BASE" =<< getBazelOutputBase
  pure workspace

getBazelOutputBase :: IO FilePath
getBazelOutputBase = do
  bazel <- getBazelPath
  lines <$> readProcess bazel ["info", "output_base"] "" <&> \case
    outputBase : _ -> outputBase
    _ -> error "failed to get output_base"

getBazelVersion :: IO (Maybe String)
getBazelVersion = do
  bazel <- getBazelPath
  let find = \case
        [] -> Nothing
        line : remaining -> case stripPrefix "Build label: " line of
          Just version -> pure version
          Nothing -> find remaining
  lines <$> readProcess bazel ["version"] "" <&> find

getBazelPath :: IO FilePath
getBazelPath = fromMaybe "bazel" <$> getSkyscopeEnv "BAZEL_BIN"
