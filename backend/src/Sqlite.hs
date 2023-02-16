{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sqlite
  ( Database
  , withDatabase
  , withStatement
  , Conflict(..)
  , executeSql
  , executeSqlScalar
  , executeStatements
  , batchInsert
  , batchInsertInternal
  , fromSQLInteger
  , fromSQLText
  ) where

import Control.Exception (Exception, bracket, catchJust, throw)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.SQLite3 (Database, Error(..), SQLData(..), SQLError(..), Statement, StepResult(..))
import qualified Database.SQLite3 as SQLite3
import GHC.Stack (HasCallStack)
import Prelude

withDatabase :: FilePath -> (Database -> IO ()) -> IO ()
withDatabase path application =
  let open = SQLite3.open $ Text.pack path
  in bracket open SQLite3.close application

withStatement :: Database -> Text -> (Statement -> IO a) -> IO a
withStatement database sql action =
  bracket (SQLite3.prepare database sql) SQLite3.finalize action

data Conflict = Conflict [SQLError]
  deriving Show

instance Exception Conflict

handleConflict :: IO a -> IO a
handleConflict action =
  attempt [] =<< addUTCTime 1 <$> getCurrentTime
  where
    attempt errors deadline = do
      action `catch` retry errors deadline
    catch = catchJust $ \case
      e@(SQLError ErrorConstraint _ _) -> Just e
      e@(SQLError ErrorLocked _ _) -> Just e
      e@(SQLError ErrorBusy _ _) -> Just e
      _ -> Nothing
    retry errors deadline e = do
      now <- getCurrentTime
      if now >= deadline || sqlError e == ErrorConstraint
        then throw $ Conflict (e : errors)
        else attempt (e : errors) deadline

executeSql :: HasCallStack => Database -> [Text] -> [SQLData] -> IO [[SQLData]]
executeSql database sql params =
  withStatement database (Text.unlines sql) $ \statement -> do
    SQLite3.bind statement params
    let fetch = SQLite3.stepNoCB statement >>= \case
          Row -> (:) <$> SQLite3.columns statement <*> fetch
          Done -> pure []
    results <- handleConflict fetch
    pure results

executeSqlScalar :: Database -> [Text] -> [SQLData] -> IO SQLData
executeSqlScalar database sql params = do
  [ [ result ] ] <- executeSql database sql params
  pure result

executeStatements :: Database -> [[Text]] -> IO ()
executeStatements database statements = for_ statements $ flip (executeSql database) []

batchInsert :: Database -> Text -> [Text] -> [[SQLData]] -> IO ()
batchInsert = batchInsertInternal 999 -- 32766

batchInsertInternal :: Int -> Database -> Text -> [Text] -> [[SQLData]] -> IO ()
batchInsertInternal varLimit database table columns rows = handleConflict $ do
  let columnCount = length columns
  let rowCount = length rows
  let batchSize = varLimit `div` columnCount
  let commaSep n = Text.intercalate ", " . replicate n
  let withInsertRows n f = flip (withStatement database) f $ Text.unlines
        [ "INSERT INTO " <> table <> "(" <> Text.intercalate ", " columns <> ")"
        , "VALUES " <> n `commaSep` ("(" <> columnCount `commaSep` "?"  <> ")") ]
  let split :: [a] -> ([[a]], [a])
      split xs = case splitAt batchSize xs of
        (ys, zs)
          | length ys < batchSize -> ([], ys)
          | length zs > batchSize -> first (ys :) (split zs)
          | length zs < batchSize -> ([ ys ], zs)
          | otherwise -> ([ ys, zs ], [])
      (batches, remainder) = split rows
      insert values statement = do
        SQLite3.bind statement values
        Done <- SQLite3.stepNoCB statement
        SQLite3.reset statement
  when (length batches > 0) $ withInsertRows batchSize $ \statement ->
    for_ batches $ \batch -> insert (concat batch) statement
  when (length remainder > 0) $ withInsertRows (length remainder) $
    insert $ concat remainder

fromSQLInteger :: SQLData -> Maybe Int64
fromSQLInteger = \case
  SQLInteger value -> Just value
  SQLNull -> Nothing

fromSQLText :: SQLData -> Maybe Text
fromSQLText = \case
  SQLText value -> Just value
  SQLNull -> Nothing
