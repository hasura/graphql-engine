{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Utils where

import qualified Data.Text      as T
import           Hasura.Prelude

dropAndSnakeCase :: T.Text -> T.Text
dropAndSnakeCase = T.drop 9 . toSnakeCase . T.toLower

toSnakeCase :: T.Text -> T.Text
toSnakeCase = T.pack . map change . T.unpack
  where
    change '-' = '_'
    change c   = c

isXHasuraTxt :: T.Text -> Bool
isXHasuraTxt = T.isInfixOf "x-hasura-" . T.toLower

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

accessKeyHeader :: T.Text
accessKeyHeader = "x-hasura-access-key"
