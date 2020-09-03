module Hasura.Sources.Postgres.SQLBuilder (postgresBuilder) where

import           Hasura.Prelude

import qualified Data.Text        as T
import qualified Text.Builder     as TB

import           Hasura.SQL.DML
import           Hasura.SQL.Text
import           Hasura.SQL.Types


postgresBuilder :: SQLBuilder
postgresBuilder = SQLBuilder
  pgLiteral
  pgIdentifier

pgLiteral :: T.Text -> TB.Builder
pgLiteral = TB.text . pgFmtLit

pgIdentifier :: Identifier -> TB.Builder
pgIdentifier = TB.text . pgFmtIden . toTxt
