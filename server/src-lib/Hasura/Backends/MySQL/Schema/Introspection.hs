module Hasura.Backends.MySQL.Schema.Introspection
  ( listAllTables,
  )
where

import Hasura.Backends.MySQL.Types (TableName)
import Hasura.Base.Error (QErr, throw500)
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName)

-- | List all tables, tracked or untracked, on a given data source.
listAllTables :: MonadError QErr m => env -> SourceName -> m [TableName]
listAllTables _ _ = throw500 "listAllTables: not implemented for MySQL backend"
