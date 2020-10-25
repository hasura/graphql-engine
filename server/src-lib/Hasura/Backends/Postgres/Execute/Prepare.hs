module Hasura.Backends.Postgres.Execute.Prepare where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.IntMap                        as IntMap
import qualified Database.PG.Query                  as Q


import           Hasura.Backends.Postgres.SQL.Value
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
