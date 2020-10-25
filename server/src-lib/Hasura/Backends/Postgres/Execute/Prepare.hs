module Hasura.Backends.Postgres.Execute.Prepare where

import           Hasura.Prelude

import qualified Data.Aeson                                    as J
import qualified Data.IntMap                                   as IntMap
import qualified Database.PG.Query                             as Q

import qualified Hasura.Backends.Postgres.SQL.DML              as S

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.Delete
import           Hasura.Backends.Postgres.Translate.Insert
import           Hasura.Backends.Postgres.Translate.Mutation
import           Hasura.Backends.Postgres.Translate.RemoteJoin
import           Hasura.Backends.Postgres.Translate.Returning
import           Hasura.Backends.Postgres.Translate.Select
import           Hasura.Backends.Postgres.Translate.Update
import           Hasura.SQL.Backend
import           Hasura.Session

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArg    = (Q.PrepArg, PGScalarValue)
type PrepArgMap = IntMap.IntMap PrepArg

withUserVars :: SessionVariables -> PrepArgMap -> PrepArgMap
withUserVars usrVars list =
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)
  in IntMap.insert 1 (prepArg, usrVarsAsPgScalar) list
