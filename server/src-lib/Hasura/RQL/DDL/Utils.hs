module Hasura.RQL.DDL.Utils
  ( clearHdbViews
  ) where

import qualified Database.PG.Query as Q

import           Hasura.Prelude
import           Hasura.SQL.Types

clearHdbViews :: Q.Tx ()
clearHdbViews = do
  -- clear views
  views <- fetchAllHdbViews
  mapM_ dropView views
  -- clear routines
  routines <- fetchAllRoutines
  mapM_ dropRoutine routines
  where
    fetchAllHdbViews =
      map runIdentity <$> Q.listQ [Q.sql|
               SELECT viewname
               FROM pg_catalog.pg_views
               WHERE schemaname = $1
               |] (Identity hdbViewsSchema) True

    fetchAllRoutines =
      map runIdentity <$> Q.listQ [Q.sql|
               SELECT routine_name
               FROM information_schema.routines
               WHERE specific_schema = $1
               |] (Identity hdbViewsSchema) True

    dropView :: TableName -> Q.Tx ()
    dropView vn = do
      let qvn = QualifiedObject hdbViewsSchema vn
          q = "DROP VIEW IF EXISTS " <> toSQLTxt qvn <> " CASCADE"
      Q.unitQ (Q.fromText q) () True

    dropRoutine :: FunctionName -> Q.Tx ()
    dropRoutine fn = do
      let qfn = QualifiedObject hdbViewsSchema fn
          q = "DROP FUNCTION " <> toSQLTxt qfn <> "() CASCADE"
      Q.unitQ (Q.fromText q) () True
