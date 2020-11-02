module Hasura.RQL.Types.SchemaCache where

import qualified Data.HashMap.Strict as M

import           Hasura.RQL.Types.RemoteSchema

data RemoteSchemaCtx

type RemoteSchemaMap = M.HashMap RemoteSchemaName RemoteSchemaCtx