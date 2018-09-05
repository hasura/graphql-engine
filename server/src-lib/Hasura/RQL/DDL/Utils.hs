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
  \        string_agg('DROP FUNCTION hdb_views.' || quote_ident(r.routine_name) || '() CASCADE;' \
  \                 , E'\n') \
  \ FROM   information_schema.routines r \
  \ WHERE  r.specific_schema = 'hdb_views'; \
  \ IF _sql IS NOT NULL THEN \
  \    EXECUTE _sql; \
  \ END IF; \
  \ END $$; "
