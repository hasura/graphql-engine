{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DDL.Utils where

import qualified Database.PG.Query as Q

clearHdbViews :: Q.Query
clearHdbViews =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views') LOOP \
   \     EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE'; \
   \   END LOOP; \
   \ END $$ "
