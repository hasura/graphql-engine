{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DDL.Utils where

import qualified Data.ByteString.Builder as BB
import qualified Database.PG.Query       as Q
import           Hasura.Prelude          ((<>))

clearHdbViews :: Q.Tx ()
clearHdbViews = Q.multiQ (Q.fromBuilder (clearHdbOnlyViews <> clearHdbViewsFunc))

clearHdbOnlyViews :: BB.Builder
clearHdbOnlyViews =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views') LOOP \
   \     EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE'; \
   \   END LOOP; \
   \ END $$; "


clearHdbViewsFunc :: BB.Builder
clearHdbViewsFunc =
  "DO $$ DECLARE \
  \ _sql text; \
  \ BEGIN \
  \ SELECT INTO _sql \
  \        string_agg(format('DROP FUNCTION %s(%s) CASCADE;' \
  \                        , p.oid::regproc \
  \                        , pg_get_function_identity_arguments(p.oid)) \
  \                 , E'\n') \
  \ FROM   pg_proc      p \
  \ JOIN   pg_namespace ns ON ns.oid = p.pronamespace \
  \ WHERE  ns.nspname = 'hdb_views'; \
  \ IF _sql IS NOT NULL THEN \
  \    EXECUTE _sql; \
  \ END IF; \
  \ END $$; "
