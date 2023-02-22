module Main where

import qualified Import
import qualified Server
import qualified Sqlite

main :: IO ()
main = do
  path <- Import.initialiseDatabase
  Sqlite.withDatabase path $ \db -> do
    Import.optimiseDatabaseAccess db
    Import.indexPathsAsync db path
    Server.server db
