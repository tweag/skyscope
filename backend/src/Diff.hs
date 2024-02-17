{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.IO.Temp (writeSystemTempFile)
import System.Process.Text (readProcessWithExitCode)

data DiffLine
  = Added String
  | Deleted String
  | Changed String (String, String)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

diff :: Text -> Text -> IO Text -- [DiffLine]
diff lhs rhs = do
  lhsPath <- writeTemp $ Text.unpack lhs
  rhsPath <- writeTemp $ Text.unpack rhs
  let change line = Text.any (== '|') line || Text.any (== '<') line || Text.any (== '>') line
  let filterChanges = Text.unlines . filter change . Text.lines
  fmap (filterChanges . LazyText.toStrict) $
    readProcessWithExitCode "diff" ["-y", lhsPath, rhsPath] "" <&> \case
      (ExitFailure code, difflines, err)
        | code == 1 -> LazyText.fromStrict difflines
        | otherwise -> error $ "diff exit " <> show code <> ": " <> Text.unpack err
      (ExitSuccess, difflines, _) -> LazyText.fromStrict difflines
  where
    writeTemp = writeSystemTempFile "skyscope.diff"

{-
  [ Added "add"
  , Deleted "delete"
  , Changed "prefix" ("from", "to")
  ]
-}
