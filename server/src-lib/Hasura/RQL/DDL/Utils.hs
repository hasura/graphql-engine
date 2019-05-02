module Hasura.RQL.DDL.Utils
  ( clearHdbViews
  ) where

import qualified Database.PG.Query as Q

import           Hasura.Prelude

-- clearHdbViews :: Q.Tx ()
-- clearHdbViews = do
--   -- clear views
--   views <- fetchAllHdbViews
--   mapM_ dropView views
--   -- clear routines
--   routines <- fetchAllRoutines
--   mapM_ dropRoutine routines
--   where
--     fetchAllHdbViews =
--       map runIdentity <$> Q.listQ [Q.sql|
--                SELECT viewname
--                FROM pg_catalog.pg_views
--                WHERE schemaname = $1
--                |] (Identity hdbViewsSchema) True

--     fetchAllRoutines =
--       map runIdentity <$> Q.listQ [Q.sql|
--                SELECT routine_name
--                FROM information_schema.routines
--                WHERE specific_schema = $1
--                |] (Identity hdbViewsSchema) True

--     dropView :: TableName -> Q.Tx ()
--     dropView vn = do
--       let qvn = QualifiedObject hdbViewsSchema vn
--           q = "DROP VIEW IF EXISTS " <> toSQLTxt qvn <> " CASCADE"
--       Q.unitQ (Q.fromText q) () True

--     dropRoutine :: FunctionName -> Q.Tx ()
--     dropRoutine fn = do
--       let qfn = QualifiedObject hdbViewsSchema fn
--           q = "DROP FUNCTION " <> toSQLTxt qfn <> "() CASCADE"
--       Q.unitQ (Q.fromText q) () True

clearHdbViews :: Q.Tx ()
clearHdbViews = Q.multiQ (Q.fromText (clearHdbOnlyViews <> clearHdbViewsFunc))

clearHdbOnlyViews :: Text
clearHdbOnlyViews =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views' ORDER BY viewname) LOOP \
   \     EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE'; \
   \   END LOOP; \
   \ END $$; "


clearHdbViewsFunc :: Text
clearHdbViewsFunc =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT routine_name FROM information_schema.routines WHERE specific_schema = 'hdb_views' ORDER BY routine_name) LOOP \
   \     EXECUTE 'DROP FUNCTION hdb_views.' || quote_ident(r.routine_name) || '() CASCADE'; \
   \   END LOOP; \
   \ END $$; "
