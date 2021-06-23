module Hasura.Backends.Postgres.Types.Table where

import           Hasura.Prelude

import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Base.Error
import           Hasura.RQL.Types.Table


mutableView
  :: (MonadError QErr m)
  => QualifiedTable
  -> (ViewInfo -> Bool)
  -> Maybe ViewInfo
  -> Text
  -> m ()
mutableView qt f mVI operation =
  unless (isMutable f mVI) $
    throw400 NotSupported $ "view " <> qt <<> " is not " <> operation
